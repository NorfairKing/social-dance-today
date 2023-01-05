echo ".backup backup.sqlite3" | ssh reserved 'cd /www/salsa-party/production/web-server && sqlite3 salsa-parties.sqlite3'
scp -O reserved:/www/salsa-party/production/web-server/backup.sqlite3 salsa-party-web-server/salsa-parties.sqlite3
