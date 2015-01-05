#!/usr/bin/python
# -* coding:utf-8 -*-
###
### FIXME: I really want to pass the lemma through
###        yy-mode feature request?

import MeCab
m = MeCab.Tagger('-Ochasen')

punct=u"!\"!&'()*+,-−./;<=>?@[\]^_`{|}~。！？…．　○●◎＊☆★◇◆"

def jp2yy (sent):
    """take a Japanese sentence in UTF8 convert to YY-mode using mecab"""
    ### (id, start, end, [link,] path+, form [surface], ipos, lrule+[, {pos p}+])
    ### set ipos as lemma (just for fun)
    ### fixme: do the full lattice
    yid = 0
    start = 0
    cfrom = 0
    cto = 0
    yy = list()
    for tok in m.parse(sent.encode('utf-8')).split('\n'):
        if tok and tok != 'EOS':
            ##print tok
            (form, p, lemma, p1, p2, p3) = tok.decode('utf-8').split('\t')
            if form in punct:
                continue
            p2 = p2 or 'n'
            p3 = p3 or 'n'
            # pos = '-'.join([p1, p2, p3])
            pos = "%s:%s-%s" % (p1, p2, p3) ## wierd format jacy requires
            cfrom = sent.find(form, cto)    ## first instance after last token
            cto = cfrom + len(form)         ## find the end
            yy.append('(%d, %d, %d, <%d:%d>, 1, "%s", %s, "null", "%s" 1.0)' % \
                (yid, start, start +1, cfrom, cto, form, 0, pos))
            yid += 1
            start += 1
    return yy


if __name__ == '__main__':
    import sys
    # thanks: http://stackoverflow.com/a/7608205/1441112
    while True:
        try:
            line = sys.stdin.readline()
        except (KeyboardInterrupt, IOError):
            break
        if not line: break
        line = line.decode('utf-8')
        print(''.join(jp2yy(line.rstrip())).encode('utf-8'))
        sys.stdout.flush()


# MeCab で遊んでみよう！
# MeCab    MeCab    MeCab    名詞-一般
# で    デ    で    助詞-格助詞-一般
# 遊ん    アソン    遊ぶ    動詞-自立    五段・バ行    連用タ接続
# で    デ    で    助詞-接続助詞
# みよ    ミヨ    みる    動詞-非自立    一段    未然ウ接続
# う    ウ    う    助動詞    不変化型    基本形
# ！    ！    ！    記号-一般
# EOS
