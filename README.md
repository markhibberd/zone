zone
====

```
Sterling Archer: Lana... Lana... Lana... LLAANNAA!
Agent Lana Kane: WHAT?
Sterling Archer: ...Danger Zone!

  - "Archer: Skytanic (#1.7)" (2010)
```

Zone is a small management tool for Amazon Route53 hosted DNS.


usage
-----

Zone configuration file format v0.

The file name is significant. It is `{hosted-zone-name}.zone`.

Example:

```
$ ls
example.com.zone
```

The syntax of the file:

```
{record-type} {ttl} {domain} {record-details...}
```

`record-details` vary for each record, examples:

```
A 300 example.com 127.0.0.1
AAAA 300 example.com ::1
CNAME 300 www.example.com example.com
MX 300 example.com 10 ASPMX.L.GOOGLE.COM
MX 300 example.com 20 ALT1.ASPMX.L.GOOGLE.COM
TXT 300 example.com "some text"
PTR 300 example.com 127.0.0.1
SRV 300 example.com 10 1 3094 127.0.0.1
SPF 300 example.com abcdef
NS 300 example.com ns1.example.com
NS 300 example.com ns2.example.com
```

Comments start with a `#`.

```
# An apex record
A 300 example.com 127.0.0.1
```

You can check the syntax of zone config file with:

```
zone check example.com.zone
```


You can apply a zone config file to amazon with:

```
export AWS_ACCESS_KEY_ID=....
export AWS_SECRET_ACCESS_KEY=....
zone apply example.com.zone
```

If you want to update you can just update and re-run.

```
zone apply example.com.zone
```



development
-----------

For any changes to apply, you will need an amazon account with
route-53 permissions.


for potential users
-------------------

Should you use this?

*No*

Can I use this?

*No*

Why?

*It is to make my life easier, and it probably won't make yours easier*
