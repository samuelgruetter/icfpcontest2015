set -e

source credentials.sh

if [ ! -f target/start ]; then
	echo "- creating start script"
	sbt start-script
else
	echo "- start script exists"
fi
echo "- computing solution"
target/start Main2 > solution
echo "- submitting"
curl --user :$API_TOKEN -X POST -H "Content-Type: application/json" \
        -d @solution \
        https://davar.icfpcontest.org/teams/$TEAM_ID/solutions
echo
echo "- done"

