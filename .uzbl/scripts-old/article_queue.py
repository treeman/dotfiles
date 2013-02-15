#!/usr/bin/env python
import os
import sys
import simplejson as json
import socket
from optparse import OptionParser
from datetime import datetime

class JSONEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, datetime):
            values = {}
            dt = {'__datetime__' : values}
            values['year'] = o.year
            values['month'] = o.month
            values['day'] = o.day
            values['hour'] = o.hour
            values['minute'] = o.minute
            values['second'] = o.second
            values['microsecond'] = o.microsecond
            values['tzinfo'] = o.tzinfo
            values['__string_repr__'] = o.isoformat()
            return dt
        else:
            return json.JSONEncoder.default(self, o)

def object_decoder(d):
    if '__datetime__' in d:
        o =  d['__datetime__']
        dt = datetime(o['year'], o['month'], o['day'], o['hour'], o['minute'], o['second'], o['microsecond'], o['tzinfo'])
        return dt
    return d

def fetch_queue():
    filename = '/home/tree/.uzbl/data/article_queue'
    if os.path.exists(filename) and os.path.getsize(filename) > 0:
        qfile = open(filename)
        return json.load(qfile, object_hook=object_decoder)
    else:
        return []

def persist_queue(queue):
    filename = '/home/tree/.uzbl/data/article_queue'
    qfile = open(filename, 'w')
    json.dump(queue, qfile, cls=JSONEncoder)

def create_url(args):
    return {'url' : args[5], 'title' : args[6], 'timestamp' : datetime.now()}

def push(args, queue):
    queue.insert(0, create_url(args))

def pop(args, queue):
    if len(queue) > 0:
        return queue.pop(0)
    fifo = open(args[3], "a")
    fifo.write("js alert('Queue empty');\n")
    fifo.close()

def shift(args, queue):
    if len(queue) > 0:
        return queue.pop()
    fifo = open(args[3], "a")
    fifo.write("js alert('Queue empty');\n")
    fifo.close()

def append(args, queue):
    queue.append(create_url(args))

def forward(args, queue):
    append(args, queue)
    return pop(args, queue)

def back(args, queue):
    push(args, queue)
    return shift(args, queue)

def list_queue(args, queue):
    qs = ""
    first = True
    for item in queue:
        if not first:
            qs += "\\n"
        first = False
        qs += item['title']
    qs = qs.replace('"', '"')
    qs = qs.replace('(', '[')
    qs = qs.replace(')', ']')
    command = "js alert(\"%s\");\n" % qs
    print command
    fifo = open(args[3], "a")
    fifo.write(command.encode('ascii', 'replace'))
    fifo.close()

def write_fifo(args, url):
    fifo = open(args[3], "a")
    if url:
        fifo.write("uri " + url['url'] + "\n")
    else:
        fifo.write("exit\n")
    fifo.close()

def dispatch_action(action, args):
    queue = fetch_queue()
    url = None
    if action == "push":
        url = push(args, queue)
    elif action == "pop":
        url = pop(args, queue)
    elif action == "shift":
        url = shift(args, queue)
    elif action == "append":
        url = append(args, queue)
    elif action == "list":
        list_queue(args, queue)
        return
    elif action == "forward":
        url = forward(args, queue)
    elif action == "back":
        url = back(args, queue)
    else:
        raise RuntimeException("Unknown action: %s" % action)
    persist_queue(queue)
    write_fifo(args, url)

def main():
    usage = "usage: %prog [options]\n\n"
    parser = OptionParser(usage=usage)
    (options, args) = parser.parse_args()
    action = args.pop()
    dispatch_action(action, args)

if __name__ == "__main__":
    main()
