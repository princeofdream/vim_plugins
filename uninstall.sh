#!/bin/bash -
#===============================================================================
#
#          FILE: uninstall.sh
#
#         USAGE: ./uninstall.sh
#
#   DESCRIPTION:
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: James Lee (JamesL), princeofdream@outlook.com
#  ORGANIZATION: BookCL
#       CREATED: 12/28/2017 09:11:19 AM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

TOP_DIR=$(pwd)

#---  FUNCTION  ----------------------------------------------------------------
#          NAME:  remove_item
#   DESCRIPTION:  remove item
#    PARAMETERS:  item
#       RETURNS:  null
#-------------------------------------------------------------------------------
remove_item ()
{
	##using unlink
	if [ -L $1 ]
	then
		echo "remove item: $1"
		unlink $1
	fi

	# if [ -d $1 ]
	# then
		# rm -rf $1
	# fi
	return 0
}

#---  FUNCTION  ----------------------------------------------------------------
#          NAME:  uninstall_plugins
#   DESCRIPTION:  uninstall plugins
#    PARAMETERS:  null
#       RETURNS:  null
#-------------------------------------------------------------------------------
uninstall_plugins ()
{
	cd $TOP_DIR/vim_plugins
	PLUGINS_LIST=`ls`

	for item in $PLUGINS_LIST; do
		# echo "get list and remove it: $item"
		remove_item ../$item
	done
	cd $TOP_DIR

}	# ----------  end of function uninstall_plugins  ----------


#---  FUNCTION  ----------------------------------------------------------------
#          NAME:  main_func
#   DESCRIPTION:  main func
#    PARAMETERS:  null
#       RETURNS:  null
#-------------------------------------------------------------------------------
main_func ()
{
	cd $TOP_DIR/vim_plugins
	uninstall_plugins $@
	cd $TOP_DIR
}	# ----------  end of function main_func  ----------


main_func $@


