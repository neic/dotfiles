#
# Remote path
#
complete -c rsync -d "Remote path" -n "commandline -ct | string match '*:*'" -a "
(
    #Prepend any user@host:/path information supplied before the remote completion
    commandline -ct | string replace -r '/[^/]*\$' '/'
)(
    #Get the list of remote files from the specified rsync server
    # The string match ensures we only complete remote paths (rsync will error out if given nothing, and stderr is ignored)
    # The string replace removes everything up to and including a time (since rsync will justify its output)
    # The grep -v removes the "." line referring to the directory
    #TODO: Replace the `grep` call with `string`
    rsync --list-only ^/dev/null (commandline -ct | string match -r '.*:(.*/)?') | string replace -r '.*[0-9]{2}:[0-9]{2}:[0-9]{2} ' '' | grep -v '^.\$' | string escape -n
)
"
