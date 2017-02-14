mkdir -p ~/.mutt/cache/headers

mkdir ~/.mutt/cache/bodies

touch ~/.mutt/certificates

touch ~/.muttrc

echo "
set imap_user = \"lijing@td.hgrica.com.cn\"
set imap_pass = \"123qweASD\"

# 这里也可以输入你的密码

#但是为了安全，还是不输

set smtp_url = \"smtp://lijing@smtp.exmail.qq.com:465/\"
set smtp_pass = \"123qweASD\"

# 这里也可以输入你的密码

#但是为了安全，还是不输。

set from = \"lijing@td.hgrica.com.cn\"

set realname = \"James Lee\" #别人收到邮件，显示的名字

set folder = \"imaps://imap.exmail.qq.com:993\"

set spoolfile = \"+INBOX\"

set postponed=\"+[Gmail]/Drafts\"

set header_cache=~/.mutt/cache/headers

set message_cachedir=~/.mutt/cache/bodies

set certificate_file=~/.mutt/certificates

set move = no
" \
	> ~/.muttrc



