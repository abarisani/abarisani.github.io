#!/bin/bash
#
# Packet-In-Packet FCS collision helper script.
#
# http://dev.inversepath.com/802.3

tweakcrc=tweakcrc # https://www.cr0.org/progs/crctool
jacksum=jacksum   # http://www.jonelo.de/java/jacksum
hexdump=hexdump

function check_args()
{
  if [ ! $# -eq 3 ]; then
    echo "Syntax: $0 packet_file inner_packet_offset collision_offset"
    echo "        $0 /tmp/packet.bin 0x200 0x10"
    exit $err
  else
    packet_file=$1
    inner_packet_offset=$(($2))
    collision_offset=$(($3))
  fi

  if $(! type $tweakcrc > /dev/null 2> /dev/null) ; then
    echo "Unable to find tweakcrc binary in current path"
    exit $err
  fi

  if $(! type $jacksum > /dev/null 2> /dev/null) ; then
    echo "Unable to find jacksum binary in current path"
    exit $err
  fi

  if $(! type $hexdump > /dev/null 2> /dev/null) ; then
    echo "Unable to find hexdump binary in current path"
    exit $err
  fi

  if [ ! -e $packet_file ]; then
    echo "Unable to find $packet_file"
    exit $err
  fi

  if ! [[ "$inner_packet_offset" =~ ^[0-9]+$ ]]; then
    echo "$2 is not a valid packet offset, this parameter must be an integer"
    exit $err
  fi

  if ! [[ "$collision_offset" =~ ^[0-9]+$ ]] || [ ! $[$collision_offset/4*4] -eq $collision_offset ]; then
    echo "$3 is not a valid collision offset, this parameter must be an integer multiple of 4"
    exit $err
  fi

  if [ "$collision_offset" -gt "$[$inner_packet_offset-4]" ]; then
    echo "collision offset must not reside within inner packet"
    exit $err
  fi
}

function backup_pktfile()
{
  echo "original packet file saved in $packet_file.bak"
  cp $packet_file $packet_file.bak
}

function extract_inner_pkt()
{
  echo "extracting inner packet from offset $inner_packet_offset"
  dd if=$packet_file of=$packet_file.inner skip=$inner_packet_offset bs=1 > /dev/null
  echo -e "extracted inner packet head\n"
  head $packet_file.inner | hexdump
  echo -e "\n"
}

function cksum_inner_pkt()
{
  checksum=$($jacksum -a crc32 -F "#CHECKSUM" $packet_file.inner)
  echo "calculating inner packet crc32 checksum: $checksum"
}

function tweak_crc()
{
  echo "altering 4 bytes at offset $collision_offset"
  echo "$tweakcrc -f $packet_file -c $checksum -o $collision_offset"
  $tweakcrc -f $packet_file -c $checksum -o $collision_offset
}

check_args $1 $2 $3
backup_pktfile
extract_inner_pkt
cksum_inner_pkt
tweak_crc
