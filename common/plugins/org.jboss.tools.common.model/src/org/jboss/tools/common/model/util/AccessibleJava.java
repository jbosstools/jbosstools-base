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

import java.io.*;
import java.util.*;
import java.util.jar.*;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;

public class AccessibleJava implements ISimpleTree {
    private static AccessibleJava instance = null;
    
    public static interface IContextPackageProvider {
    	public void setObject(XModelObject object);
    	public String getContextPackage();
    }

    public static AccessibleJava getInstance() {
        if(instance == null) instance = new AccessibleJava();
        return instance;
    }

    public static String getClassPath() {
        StringBuffer sb = new StringBuffer();
        String[] s = new String[0]; 
        for (int i = 0; i < s.length; i++) sb.append(File.pathSeparator).append(s[i]);
        return sb.toString();
    }

    private Map<String,JarFile> jarsmap = null;
    private Map<String,SortedSet<String>> map = null;
    private File[] folders = null;
    private HashSet<String> checkedpackages = new HashSet<String>();

    private AccessibleJava() {
        load();
    }

    private String getJavaLib() {
        String rt = System.getProperty("java.home") + "/lib/rt.jar";
        return rt;
    }

    protected String extension() {
        return ".class";
    }

    private void load() {
        map = new HashMap<String,SortedSet<String>>();
        jarsmap = new HashMap<String,JarFile>();
        addJars(getJavaLib());
        String paths = getClassPath();
        addJars(paths);
        Iterator c = jarsmap.values().iterator();
        while(c.hasNext()) loadJar((JarFile)c.next());
        jarsmap = null;
        addFolders(paths);
    }

    private void addJars(String paths) {
        StringTokenizer st = new StringTokenizer(paths, File.pathSeparator);
        while(st.hasMoreTokens()) {
            String t = st.nextToken().trim();
            try { 
            	addJar(new JarFile(t)); 
            } catch (Exception e) {
            	ModelPlugin.log("addJars:" + e.getClass().getName());
            }
        }
    }

    private void addJar(JarFile jf) throws IOException {
        String n = new File(jf.getName()).getCanonicalPath();
        if(jarsmap.get(n) != null) return;
        jarsmap.put(n, jf);
        addJarManifest(jf);
    }

    private void loadJar(JarFile jar) {
        Enumeration e = jar.entries();
        while(e.hasMoreElements()) {
            Object o = e.nextElement();
            if(!(o instanceof JarEntry)) continue;
            JarEntry je = (JarEntry)o;
            register(je.getName(), map);
        }
        try { 
        	jar.close(); 
        } catch (Exception exc) {
        	ModelPlugin.log("AccessibleJava:Cannot close jar.");
        }
    }

    private static String[] serviceroots = {"CVS/", "META-INF/", ".svn"};

    private void register(String path, Map<String,SortedSet<String>> map) {
        if(path == null || path.indexOf("$") >= 0 || path.equals("/")) return;
        for (int j = 0; j < serviceroots.length; j++)
          if(path.indexOf(serviceroots[j]) >= 0) return;
        boolean isPackage = path.endsWith("/");
        if(!(isPackage || path.endsWith(extension()))) return;
        if(path.startsWith("/")) path = path.substring(1);
        path = path.substring(0, path.length() - ((isPackage) ? 1 : 6)).replace('/', '.');
        register0(path, isPackage, map);
    }

    public String[] getContent(String packagename) {
        checkfolders(packagename);
        SortedSet<String> list = map.get(packagename);
        return (list == null) ? null : list.toArray(new String[list.size()]);
    }

    private void register0(String path, boolean isPackage, Map<String,SortedSet<String>> map) {
        String parent, name;
        int i = path.lastIndexOf('.');
        if(i > 0) {
            parent = path.substring(0, i);
            name = path.substring(i + 1);            
        } else {
            parent = "%root%";
            name = path;
        }
        if(!isJavaName(name)) return;
        SortedSet<String> list = map.get(parent);
        if(list == null) {
            list = new TreeSet<String>();
            map.put(parent, list);
        }
        String pname = name + ".";
        if(list.contains(name) || list.contains(pname)) return;
        list.add((isPackage) ? pname : name);
        if(i > 0) register0(parent, true, map);
    }
    
    boolean isJavaName(String name) {
    	if(name.length() == 0) return false;
    	if(!Character.isJavaIdentifierStart(name.charAt(0))) return false;
    	for (int i = 1; i < name.length(); i++) {
        	if(!Character.isJavaIdentifierPart(name.charAt(i))) return false;
    	}
    	return true;
    }

    private void addFolders(String paths) {
        StringTokenizer st = new StringTokenizer(paths, File.pathSeparator);
        ArrayList<File> l = new ArrayList<File>();
        while(st.hasMoreTokens()) {
            String t = st.nextToken().trim();
            if(t.length() == 0 || t.indexOf('~') >= 0) continue;
            File f = new File(t);
            if(f.isDirectory()) l.add(f);
        }
        folders = l.toArray(new File[l.size()]);
        for (int i = 0; i < folders.length; i++) {
            File[] fs = folders[i].listFiles();
            if(fs != null) for (int j = 0; j < fs.length; j++) {
                if(fs[j].isDirectory()) register(fs[j].getName() + "/", map);
            }
        }
    }

    private void checkfolders(String pkg) {
        if(checkedpackages.contains(pkg)) return;
        checkedpackages.add(pkg);
        for (int i = 0; i < folders.length; i++) loadfolder(folders[i], pkg);
        int i = pkg.lastIndexOf('.');
        if(i >= 0) checkfolders(pkg.substring(0, i));
    }

    private void loadfolder(File f, String pkg) {
        String p = pkg.replace('.', '/');
        File pf = new File(f.getAbsolutePath() + '/' + p);
        if(!pf.exists()) return;
        File[] fs = pf.listFiles();
        if(fs != null) for (int i = 0; i < fs.length; i++) {
            String n = fs[i].getName();
            if(fs[i].isDirectory()) register(p + '/' + n + '/', map);
            else if(n.endsWith(extension())) register(p + '/' + n, map);
        }
    }

    private void addJarManifest(JarFile jf) throws IOException {
        Manifest m = jf.getManifest();
        Attributes as = m.getMainAttributes();
        String classpath = as.getValue("Class-Path");
        if(classpath == null) return;
        String r = new File(jf.getName()).getParentFile().getCanonicalPath() + File.separatorChar;
        StringTokenizer st = new StringTokenizer(classpath, " ");
        StringBuffer sb = new StringBuffer();
        while(st.hasMoreElements()) {
            String t = st.nextToken().trim();
            if(t.length() > 0) sb.append(r).append(t).append(File.pathSeparator);
        }
        addJars(sb.toString());
    }
}

