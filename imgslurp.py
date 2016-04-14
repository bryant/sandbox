import requests

def get_album_name(s):
    return s

def image_filenames(album):
    url = "http://imgur.com/ajaxalbums/getimages/%s/?all=true" % album
    js = requests.get(url).json()
    return (image["hash"] + image["ext"] for image in js["data"]["images"])

if __name__ == "__main__":
    from sys import argv
    for im in image_filenames(get_album_name(argv[1])):
        print "http://i.imgur.com/" + im
