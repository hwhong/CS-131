import datetime
import asyncio
import aiohttp
import time
import json
import sys
import re

servers = {
    'Goloman': 12001,
    'Hands': 12002,
    'Holiday': 12003,
    'Welsh': 12004,
    'Wilkes': 12005
}

servers_rel = {
    'Goloman': ['Hands', 'Holiday', 'Wilkes'],
    'Holiday': ['Welsh', 'Goloman', 'Wilkes'],
    'Wilkes': ['Goloman', 'Hands', 'Holiday'],
    'Hands': ['Goloman', 'Wilkes'],
    'Welsh': ['Holiday']
}
google_url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
google_api = "AIzaSyDdR5OQFWq9Ob_kBCxatmMD7N3tmp3RkI8"

client_cache = dict() # client_id => [IAMAT, client_id, +lat-lon, client_time]

# ---------------------
# AT
# ---------------------
async def handle_at(msg, writer, unparsed_msg):
	# AT server_id time_diff client_id +lat-lon time
	client_id = msg[1]
	time = msg[3]

	# the information is new, therefore this server needs flooding
	if client_id not in client_cache or float(time) > float(client_cache[client].split(" ")[3]):	
		log_file.write(f"Updating servers by flooding.\n")
		await handle_flood(unparsed_msg)
	else:
		log_file.write(f"Flooding is unncessary.\n")
   
# ---------------------
# FLOODING ALGO
# ---------------------
async def handle_flood(unparsed_msg):
	for neighbor in servers_rel[sys.argv[1]]:
		log_file.write(f"Server {sys.argv[1]} attempting to connect with server {neighbor}\n")
		try: 
			reader, writer = await asyncio.open_connection('127.0.0.1', servers[neighbor], loop = loop)

			log_file.write(f"Server {sys.argv[1]} connects with server {neighbor}\n")

			writer.write(unparsed_msg.encode())
			log_file.write(f"Server {sys.argv[1]} sends message {unparsed_msg}\n")
			# obligatory draining after write
			await writer.drain()

			writer.write_eof()
			log_file.write(f"Server {sys.argv[1]} closes connection with server {neighbor}\n")
			writer.close()
		except Exception as inst:
			log_file.write(f"Server {sys.argv[1]} failed to connect with server {neighbor} due to {type(inst)}\n")

# ---------------------
# PARSE LATLONG Helper
# ---------------------
def latlong(coords):
	# input sanity check
	symb_counts = 0
	for c in coords: 
		if c == "+" or c == "-":
			symb_counts+=1
	if symb_counts != 2:
		return None

	i = 0 
	# the last signed symbol
	for idx in range(len(coords)):
		if coords[idx] == "+" or coords[idx] == "-":
			i = idx

	return [coords[:i],coords[i:]]

# ---------------------
# WHATSAT
# possibly need to be ran in different thread, loop.run_in_executor()
# ---------------------
async def handle_whatsat(msg, writer):
	# WHATSAT client_id radius upper_bound

	bad_input = False
	# check for bad inputs 
	if len(msg) != 4:
		bad_input = True
		log_file.write(f"Invalid WHATSAT message: incorrect number of params\n")

	# radius or upper_bound not numerical 
	if not msg[2].isdigit() or not msg[3].isdigit():
		bad_input = True
		log_file.write(f"Invalid WHATSAT message: radius or upper_bound not numerical\n")

	# radius or upper_bound not in limit
	if float(msg[2]) > 50 or float(msg[2]) <= 0 or float(msg[3]) > 20:
		bad_input = True
		log_file.write(f"Invalid WHATSAT message: radius or upper_bound not in limit\n")

	# short-circuit if invalid command 
	if bad_input:
		unparsed_msg = " ".join(msg)
		writer.write(f"? {unparsed_msg}".encode())
		log_file.write(f"? {unparsed_msg}\n")
		await writer.drain()
		writer.write_eof()
		return False

	# grab cached_msg so we can construct output 
	# [IAMAT, client_id, +lat-lon, client_time]
	cached_msg = client_cache[msg[1]]

	# AT server_id time_diff client_id +lat-lon time JSON

	server_time = float(time.mktime(datetime.datetime.now().timetuple()))
	client_time = float(cached_msg[3])
	time_diff = client_time - server_time
	if time_diff > 0:
		time_diff = "+" + str(time_diff)

	client_copy = " ".join(cached_msg[1:])

	# Google places API. Code partially obtained from Discussion 1C slides
	coords = latlong(cached_msg[2])
	url = google_url + f'location={coords[0]},{coords[1]}&radius={str(float(msg[2]) * 1000)}&key={google_api}'
	async with aiohttp.ClientSession(connector=aiohttp.TCPConnector(ssl=False)) as session:
		async with session.get(url) as resp: 
			# TODO: THIS WORKS, just over daily quota. need to test again
			data = await resp.json()
			# sequence of two or more adjacent newlines is replaced by a single newline 
			# followed by two new lines 
			indented_data = json.dumps(data, indent = 3) + "\n\n"
			output_msg = (f"AT {sys.argv[1]} {time_diff} {client_copy}\n{indented_data}")

			log_file.write(f"SENDS message {output_msg}")

			# sends client response
			writer.write(output_msg.encode())
			await writer.drain()
			writer.write_eof()

			return True

# ---------------------
# IAMAT
# ---------------------
async def handle_iamat(msg, writer):
	# IAMAT client_id +lat-lon client_time

	bad_input = False
	# check for bad inputs (lacking args or incorrect client_time)
	if len(msg) != 4 or not bool(re.match(r"^[0-9\.]*$", msg[3])):
		bad_input = True
		log_file.write(f"Invalid IAMAT message\n")

	# short-circuit if invalid command 
	if bad_input:
		unparsed_msg = " ".join(msg)
		writer.write(f"? {unparsed_msg}".encode())
		log_file.write(f"? {unparsed_msg}\n")
		await writer.drain()
		writer.write_eof()
		return False

	# AT server_id time_diff client_id +lat-lon time

	server_time = float(time.mktime(datetime.datetime.now().timetuple()))
	client_time = float(msg[3])
	time_diff = client_time - server_time
	if time_diff > 0:
		time_diff = "+" + str(time_diff)

	client_copy = " ".join(msg[1:])
	output_msg = (f"AT {sys.argv[1]} {time_diff} {client_copy}")

	# cache client info for future
	client_cache[msg[1]] = msg

	log_file.write(f"SENDS message {output_msg} to client {msg[1]}\n")

	# sends client response
	writer.write(output_msg.encode())
	await writer.drain()
	writer.write_eof()

	# need to flood to other servers
	log_file.write(f"Flooding Information to other servers\n")
	await handle_flood(output_msg)

	return True

# ---------------------
# Handling Input
# ---------------------
async def handle_connection(reader, writer):
	decoded_msg = (await reader.readline()).decode()
	
	log_file.write(f"RECEIVED message {decoded_msg}")
	parsed_msg = decoded_msg.strip().split()

	# CLIENT <-> SERVER
	if parsed_msg[0] == "IAMAT": 
		log_file.write(f"Processing IAMAT message\n")
		await handle_iamat(parsed_msg, writer)

	# CLIENT <-> SERVER
	if parsed_msg[0] == "WHATSAT": 
		log_file.write(f"Processing WHATSAT message\n")
		await handle_whatsat(parsed_msg, writer)

	# SERVER <-> SERVER
	if parsed_msg[0] == "AT":
		log_file.write(f"Processing custom AT message\n")
		await handle_at(parsed_msg, writer, decoded_msg)

	await writer.drain()
	writer.close()

# Code partially obtained from Discussion 1C Slides
def main():

	# e.g python3 server.py Goloman
	if len(sys.argv) != 2:
		print("Error: Bad args")
		sys.exit(1)
	if sys.argv[1] not in servers:
		print("Error: Bad server name")
		sys.exit(1)
	
	global log_file
	log_file = open(sys.argv[1] + "_log.txt", "w+")
	log_file.write("---  " + sys.argv[1] + " starts ---\n")
	
	global loop
	# Gets current event loop, or creates new one
	loop = asyncio.get_event_loop()
	# Start a socket server with the coroutine client_accept
	coro = asyncio.start_server(handle_connection, '127.0.0.1', servers[sys.argv[1]], loop=loop)
	server = loop.run_until_complete(coro)

	try:
		loop.run_forever()
	except KeyboardInterrupt:
		pass
	
	log_file.write("---  " + sys.argv[1] + " closes ---\n")

	server.close()
	loop.run_until_complete(server.wait_closed())
	loop.close()
	log_file.close()

if __name__ == '__main__':
	main()
