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

import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.IParent;
import org.eclipse.jdt.core.JavaModelException;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class AccessibleClasses implements ISimpleTree {
    private static final int JAVA = 1;
    private static final int PROJECT = 2;
    private int mode = 2;
    private XModel model;
    IJavaProject javaProject;
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
        javaProject = EclipseResourceUtil.getJavaProject(EclipseResourceUtil.getProject(model.getRoot()));
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
        try {
        	exists = getChildren(list, packagename);
        } catch (JavaModelException e) {
        	ModelPlugin.getPluginLog().logError(e);
        }
        return exists;
    }
    
    private boolean getChildren(SortedSet<String> list, String packagename) throws JavaModelException {
    	if(javaProject == null || !javaProject.exists()) return false;
        boolean exists = false;
        boolean r = "%root%".equals(packagename);
        String jp = (r) ? "" : packagename;
    	IPackageFragmentRoot[] rs = javaProject.getPackageFragmentRoots();
    	for (int i = 0; i < rs.length; i++) {
    		IParent pf = (r) ? rs[i] : rs[i].getPackageFragment(packagename);
    		if(pf == null || !((IJavaElement)pf).exists()) continue;
    		exists = true;
    		IJavaElement[] cs = pf.getChildren();
    		process(list, cs, jp);
    		if(!r) process2(list, rs[i].getChildren(), jp);
    	}
    	return exists;
    }
    
    private void process(SortedSet<String> list, IJavaElement[] cs, String jp) throws JavaModelException {
        for (int j = 0; j < cs.length; j++) {
            boolean isp = cs[j] instanceof IPackageFragment;
            String n = cs[j].getElementName();
            if(n.length() == 0 && isp) {
            	process(list, ((IPackageFragment)cs[j]).getChildren(), jp);
            } else {
            	if(isp) {
            		if(n.indexOf('.') >= 0) continue;
            		n += ".";
            	} else {
            		int d = n.lastIndexOf('.');
            		if(d >= 0) {
            			String ext = n.substring(d + 1);
            			n = n.substring(0, d);
            			if(!extensions().contains("." + ext + ".")) continue;
            		}
            	}
            	if(accepts(jp, n)) {
            		list.add(n);
            	}
            }
        }
    }
    private void process2(SortedSet<String> list, IJavaElement[] cs, String jp) throws JavaModelException {
        for (int j = 0; j < cs.length; j++) {
        	String n = cs[j].getElementName();
        	if(!n.startsWith(jp + ".")) continue;
        	n = n.substring(jp.length() + 1);
            boolean isp = cs[j] instanceof IPackageFragment;
        	if(isp) {
        		if(n.indexOf('.') >= 0) continue;
        		n += ".";
        	} else {
        		int d = n.lastIndexOf('.');
        		if(d >= 0) {
        			String ext = n.substring(d + 1);
        			n = n.substring(0, d);
        			if(!extensions().contains("." + ext + ".")) continue;
        		}
        	}
        	if(accepts(jp, n)) {
        		list.add(n);
        	}
        }
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

