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
    argparser.add_argument('xml')
    argparser.add_argument('head_sdf')
    argparser.add_argument('hat_sdf')
    argparser.add_argument('out_obj_path')
    argparser.add_argument('smurf_exec')

    args = argparser.parse_args()
    filename = args.xml
    l = json_from_xml(filename)

    os.system("{} \"{}\" {} {} {}".format(args.smurf_exec, json.dumps(l), args.head_sdf, args.hat_sdf, args.out_obj_path))
