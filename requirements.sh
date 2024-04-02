#!/usr/bin/env sh

if ! command -v cobc &> /dev/null
then
    echo -e "[\e[31m✗\e[0m] GNU COBOL"
    exit 1
else
    echo -e "[\e[32m✓\e[0m] GNU COBOL"
fi

echo "All requirements met!"
exit 0
