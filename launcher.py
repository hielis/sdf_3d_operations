import json
import argparse
import os
import xml.etree.ElementTree as ET

def json_from_xml(filename):
        tree = ET.parse(filename)
        root = tree.getroot()
        face_point = root.iter()
        i = 0
        res = []
        for c in root:
                if (c.tag == 'face_points_all'):
                        for d in c:
                                h = d.attrib
                                e = {'x' : h['v0'] , 'y' :  h['v1'] , 'z' :  h['v2']}
                                res.append(e)
        return res

if __name__ == "__main__":
        argparser = argparse.ArgumentParser()
        argparser.add_argument('filename')
        argparser.add_argument('hat_sdf')
        argparser.add_argument('head_sdf')
        args = argparser.parse_args()
        filename = args.filename
        l = json_from_xml(filename)
	os.system("{} \"{}\" {} {} {}".format("./smurf", json.dumps(l), args.hat_sdf, args.head_sdf, "out.obj"))
