
GDRIVE := /run/user/1000/gvfs/google-drive:host=gmail.com,user=YOURUSER/THEFILEPATH

REFDIR := network

network:
	ln -s ${GDRIVE} $@

