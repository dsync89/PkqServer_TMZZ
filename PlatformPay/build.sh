#!/bin/bash
svn up

project_path=$(pwd)
publish_root=/Users/zxystar/Project_TK/project/tags/91_server/
version=1.3.0
package_name=AccountServer_${version}_$(date +"%Y%m%d%H%M")
publish_path=${publish_root}/${package_name}

cd ./deps
rm ./ebin/*.beam
erl -make
cd ..
rm ./ebin/*.beam
erl -make
cp ./deps/ebin/*.beam ./ebin/
cd $publish_root

if [ -d ${publish_path} ]; then
	rm -rf ${publish_path}
fi
mkdir ${publish_path}

cp -rf ${project_path}/ebin ./${package_name}
cp -rf ${project_path}/config ./${package_name}
cp -rf ${project_path}/script ./${package_name}

find ./${package_name} -type d -name ".svn" |xargs rm -rvf;
find . -type f -name ".DS_Store" |xargs rm -rvf;

tar -zcvf ./${package_name}.tar.gz ./${package_name}
rm -rdf ./${package_name}
svn add  ./${package_name}.tar.gz
svn commit ./${package_name}.tar.gz -m "${package_name}"



