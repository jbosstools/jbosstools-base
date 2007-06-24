/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.util;

import java.util.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;

public class FindObjectHelper implements SpecialWizard {
	public static int EVERY_WHERE = 0;
	public static int IN_EDITOR_ONLY = 1;
	public static int IN_NAVIGATOR_ONLY = 2;
	public static int IN_NAVIGATOR_AND_IN_EDITOR_IF_OPEN = 3;
	
    static SpecialWizard select = createSelectWizard();
    private XModel model = null;
    private String command = null;
    private int off = -1;
    private FileInfo file = new FileInfo();
    private SegmentInfo segment = new SegmentInfo();
    private int line = -1;

    public static SpecialWizard createSelectWizard() {
        return SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.select.SelectObjectWizard");
    }

    public static int findModelObject(XModelObject o, int where) {
    	if(o == null) return 1;
        SpecialWizard wizard = createSelectWizard();
        wizard.setObject(new Integer(where));
        wizard.setObject(o);
        return wizard.execute();
    }

    public static int findModelObject(XModelObject o, int where, String preferredPage) {
    	if(o == null) return 1;
        SpecialWizard wizard = createSelectWizard();
        wizard.setObject(new Integer(where));
        wizard.setObject(o);
        wizard.setObject(preferredPage);
        return wizard.execute();
    }

    public static String makeRef(XModelObject o) {
        return "" + o.getAttributeValue("element type") + " " + makeRef(o.getPath(), o.getPresentationString());
    }

    public static String makeRef(XModelObject o, int line) {
        return "" + o.getAttributeValue("element type") + " " + makeRef(o.getPath() + ":" + line, o.getPresentationString());
    }

    public static String makeRef(String hidden, String visible) {
        return "@" + hidden + "@" + visible + "@";
    }

    public FindObjectHelper() {}

    public void setObject(Object object) {
        Object[] os = (Object[])object;
        model = (XModel)os[0];
        command = (String)os[1];
        off = ((Integer)os[2]).intValue();
    }

    public int execute() {
        XModelObject o = findRef();
        if(o == null) o = findJava();
        if(o != null) {
            o.set("_error_line_", "" + line);
            select.setObject(o);
            select.execute();
            o.set("_error_line_", "");
        }
        return 0;
    }

    private XModelObject findRef() {
        int i = 0;
        while(i < command.length()) {
            int i1 = command.indexOf("@", i);
            if(i1 < 0 || i1 > off) return null;
            int i2 = command.indexOf("@", i1 + 1);
            if(i2 < 0) return null;
            String p = command.substring(i1 + 1, i2);
            int i3 = command.indexOf("@", i2 + 1);
            if(i3 < 0) return null;
            if(i3 >= off) {
                int q = p.indexOf(":");
                if(q >= 0) {
                    try {
                    	line = Integer.parseInt(p.substring(q + 1));
                    } catch (Exception e) {
                    	//ignore
                    }
                    p = p.substring(0, q);
                }
                return model.getByPath(p);
            }
            i = i3 + 1;
        }
        return null;
    }

    private XModelObject findJava() {
        JavacErrorHeadLine h = new JavacErrorHeadLine(model, command);
        if(h.jf() != null) file.setData(h.jf(), h.line(), model);
        else if(h.cls() != null) file.setData(h.cls(), h.line(), model);
        else return null;
        XModelObject o = model.getByPath(file.getJPath());
        line = file.getLine() - 1;
        if (o != null) return o;
        if(file.getText().length() == 0) return null;
        segment.setData(file);
        segment.parse();
        String path = segment.getPath();
        if(path == null) return null;
        o = model.getByPath(path);
        line = segment.getLine();
        return o;
    }

    public static String enhanceStackTraceEntry(XModel model, String st) {
        StackTraceLine q = new StackTraceLine(st);
        if(q.cls() == null) return st;
        FileInfo f = new FileInfo();
        f.setData(q.cls(), q.line() + 1, model);
        XModelObject o = model.getByPath(f.getJPath());
        if(o != null) {
            return q.getLineStart() + makeRef(o.getPath() + ":" + q.line(), q.toString());
        } else if(q.line() < 0) {
            return st;
        }
        SegmentInfo s = new SegmentInfo();
        s.setData(f);
        s.parse();
        String path = s.getPath();
        o = (path == null) ? null : model.getByPath(path);
        if(o == null) return st;
        return q.getLineStart() + makeRef(o.getPath() + ":" + s.getLine(), q.toString());
    }

}

class FileInfo {
    private String text = "";
    private int line;
    private String jpath = "";

    public void setData(String cls, int ln, XModel model) {
        jpath = "/" + cls.replace('.', '/') + ".java";
        line = ln;
        String fn = "" + XModelObjectUtil.getExpandedValue(model.getByPath("Engines/generator"), "directory", null) + "/src" + jpath;
        text = XModelObjectLoaderUtil.readFile(fn);
    }

    public void setData(XModelObject cls, int ln, XModel model) {
        jpath = cls.getPath();
        line = ln;
        text = cls.getAttributeValue("body");
    }

    public String getJPath() {
        return jpath;
    }

    public String getText() {
        return text;
    }

    public int getLine() {
        return line;
    }

}

class SegmentInfo {
    private FileInfo file;
    private String path = null;
    private String attr = null;
    int line = -1;

    public SegmentInfo() {}

    public void setData(FileInfo file) {
        this.file = file;
    }

    public void parse() {
        path = null;
        String text = file.getText();
        line = file.getLine();
        int off = getOffset(text, line);
        int b = 0, e = 0;
        while(true) {
            b = text.indexOf("//<!--", e);
            if(b < 0) return;
            e = text.indexOf("//--", b);
            if(e < 0) return;
            if(e > off) break;
        }
        b = text.indexOf("//", b + 2);
        if(b < 0) return;
        int b2 = text.indexOf("\n", b);
        if(b2 < 0) return;
        path = text.substring(b + 2, b2);
        int q = path.lastIndexOf('.');
        attr = path.substring(q + 1);
        if(q >= 0) path = path.substring(0, q);
        reduceLine(text, b2 + 1);
    }

    private int getOffset(String text, int ln) {
        StringTokenizer st = new StringTokenizer(text, "\n", true);
        int r = 0;
        while(ln > 1 && st.hasMoreTokens()) {
            String s = st.nextToken();
            if("\n".equals(s)) --ln;
            r += s.length();
        }
        return r;
    }

    private void reduceLine(String text, int b) {
        StringTokenizer st = new StringTokenizer(text, "\n", true);
        int r = 0;
        while(r < b && st.hasMoreTokens()) {
            String s = st.nextToken();
            if("\n".equals(s)) --line;
            r += s.length();
        }
        --line;
    }

    public String getPath() {
        return path;
    }

    public String getAttr() {
        return attr;
    }

    public int getLine() {
        return line;
    }

}

class StackTraceLine {
    public static String PREFIX = "\tat ";
    private String beg = null;
    private String cls = null;
    private String mtd = null;
    private int ln = -1;

    public StackTraceLine(String text) {
        int i = text.indexOf(PREFIX);
        if(i < 0) return;
        beg = text.substring(0, i);
        text = text.substring(i + PREFIX.length()).trim();
        int ob = text.indexOf('(');
        if(ob < 0) return;
        int cb = text.indexOf(')', ob);
        if(cb < 0) return;
        int ld = text.lastIndexOf('.', ob);
        if(ld < 0) return;
        cls = text.substring(0, ld);
        mtd = text.substring(ld + 1, ob);
        int sc = text.indexOf(':', ob);
        if(sc < 0) sc = ob;
        String sln = text.substring(sc + 1, cb);
        try {
        	ln = Integer.parseInt(sln) - 1;
        } catch (Exception e) {
        	//ignore
        }
    }

    public String toString() {
        return cls + "." + mtd + "(" + (ln + 1) + ")";
    }

    public String getLineStart() {
        return beg + PREFIX;
    }

    public String cls() {
        return cls;
    }

    public int line() {
        return ln;
    }

    public void setLine(int line) {
        ln = line;
    }

}

class JavacErrorHeadLine {
    private String cls = null;
    private int ln = -1;
    private XModelObject jf = null;

    public JavacErrorHeadLine(XModel model, String command) {
        int i = command.indexOf(".java:");
        if(i < 0) return;
        int j = command.indexOf(':', i + 6);
        try {
        	ln = Integer.parseInt(command.substring(i + 6, j));
        } catch (Exception e) {
        	//ignore
        }
        String fn = command.substring(0, i);
        fn = fn.substring(fn.indexOf(']') + 1).trim(); // Cut off ant task prefix.
        XModelObject g = model.getByPath("Engines/generator");
        if(g != null) {
            String p = "" + XModelObjectUtil.getExpandedValue(g, "directory", null) + "/src/";
            cls = fn.substring(p.length()).replace('/', '.').replace('\\', '.');
        }
        findJFile(model, "/" + fn.replace('\\', '/') + ".java");
    }

    private void findJFile(XModel model, String fn) {
        jf = model.getByPath(fn);
        if(isOverlapped(jf)) jf = null;
        if(jf != null) return;
        int i = fn.indexOf('/', 1);
        if(i >= 0) findJFile(model, fn.substring(i));
    }

    private boolean isOverlapped(XModelObject o) {
        while(o != null) {
            if("true".equals(o.get("overlapped"))) return true;
            o = o.getParent();
        }
        return false;
    }

    public String cls() {
        return cls;
    }

    public XModelObject jf() {
        return jf;
    }

    public int line() {
        return ln;
    }

}

