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
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.filesystems.XFileObject;

public class AccessibleClasses implements ISimpleTree {
    private static final int JAVA = 1;
    private static final int PROJECT = 2;
    private int mode = 2;
    private XModel model;
    private Map<String,SortedSet<String>> map = null;
    private static SortedSet<String> EMPTY = new TreeSet<String>();
    private Comparator<String> comparator = new ACComparator<String>();

    public AccessibleClasses(XModel model, boolean useproject) {
        this(model, (useproject && model != null) ? 3 : 1);
    }

    protected XModel getModel() {
        return model;
    }

    public AccessibleClasses(XModel model, int mode) {
        this.model = (XModel)model;
        this.mode = mode;
        map = ((mode & PROJECT) != 0) ? new HashMap<String,SortedSet<String>>() : null;
    }

    public String[] getContent(String packagename) {
        if(map == null) return AccessibleJava.getInstance().getContent(packagename);
        SortedSet<String> o = map.get(packagename);
        if(o == null) {
            load(packagename);
            o = map.get(packagename);
        }
        if(o == EMPTY) return null;
        String[] r = o.toArray(new String[o.size()]);
        Arrays.sort(r, comparator);
        return r;
    }

    protected void load(String packagename) {
        SortedSet<String> list = new TreeSet<String>();
        String[] s = ((mode & JAVA) == 0) ? null :
                     AccessibleJava.getInstance().getContent(packagename);
        if(s != null) for (int i = 0; i < s.length; i++) list.add(s[i]);
        boolean exists = buildPackage(list, packagename);
        if(s == null && !exists) {
            map.put(packagename, EMPTY);
        } else {
            map.put(packagename, list);
        }
    }

    private static String exts = ".class.bo.java.cls.";

    protected String extensions() {
        return exts;
    }

    private boolean buildPackage(SortedSet<String> list, String packagename) {
    	String pkg = packagename.toLowerCase();
        if(pkg.endsWith(".cvs")) return false;
        if(pkg.endsWith(".svn")) return false;
        boolean exists = false;
        XModelObject fs = FileSystemsHelper.getFileSystems(model);
        if(fs == null) return false;
        XModelObject[] rs = fs.getChildren();
        boolean r = "%root%".equals(packagename);
        String jp = (r) ? "" : packagename;
        if(!r) packagename = packagename.replace('.', '/');
        for (int i = 0; i < rs.length; i++) {
            XModelObject o = (r) ? rs[i] : rs[i].getChildByPath(packagename);
            if(o == null) continue;
            exists = true;
            XModelObject[] os = o.getChildren();
            for (int j = 0; j < os.length; j++) {
                if("true".equals(os[j].get("overlapped"))) continue;
                String ext = "." + os[j].getAttributeValue("extension") + ".";
                boolean isc = (extensions().indexOf(ext) >= 0);
                boolean isp = (os[j].getFileType() >= XFileObject.FOLDER);
                if(!isc && !isp) continue;
                String n = os[j].getAttributeValue("name");
                if(isp) n += ".";
                if(accepts(jp, n)) list.add(n);
            }
        }
        return exists;
    }

    protected boolean accepts(String packagename, String n) {
        return !n.equalsIgnoreCase("CVS.") && !n.equalsIgnoreCase("svn.");
    }

}

class ACComparator<T> implements Comparator<T> {
    public int compare(Object o1, Object o2) {
        String s1 = (String)o1;
        String s2 = (String)o2;
        if(s1.endsWith(".") && !s2.endsWith(".")) return -1;
        if(s2.endsWith(".") && !s1.endsWith(".")) return 1;
        return s1.compareToIgnoreCase(s2);
    }

    public boolean equals(Object obj) {
        return this == obj;
    }

}

