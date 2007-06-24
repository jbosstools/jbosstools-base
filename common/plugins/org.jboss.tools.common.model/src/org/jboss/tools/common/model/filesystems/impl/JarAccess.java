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
package org.jboss.tools.common.model.filesystems.impl;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.jboss.tools.common.util.FileUtil;

public class JarAccess {
    private String location = null;
    private String templocation = null;
    private ZipFile jar = null;
    private HashMap<String,HashSet<String>> map = new HashMap<String,HashSet<String>>();
    private long timeStamp = -1;
    private long size = -1;

    public JarAccess() {}

    public void setLocation(String location) {
        this.location = location;
        validate();
    }
    
    public String getLocation() {
    	return location;
    }

    public boolean isLoaded() {
        return (jar != null || loading);
    }
    
    private boolean loading = false;

    public void validate() {
        if (isLoaded()) return;
		loading = true;
		templocation = null;
        try {
            File f = File.createTempFile("efs_", ".jar");
			f.deleteOnExit();
            int ind = location.indexOf(":/");
            if (ind != 1 && ind != -1) {
                InputStream i = new java.net.URL(location).openConnection().getInputStream();
                FileOutputStream o = new FileOutputStream(f);
                FileUtil.copy(i, o);
                timeStamp = -1;
                size = -1;
            } else {
            	File nf = new File(location);
                FileUtil.copyFile(nf, f, true);
                timeStamp = nf.lastModified();
                size = nf.length();
            }
            templocation = f.getCanonicalPath();
			jar = new ZipFile(f);
            init();
        } catch (Exception e) {
            timeStamp = -1;
            size = -1;
            return;
        } finally {
			loading = false;
        }
    }

    private void init() {
        map.clear();
        map.put("", new HashSet<String>());
        Enumeration en = jar.entries();
        while(en.hasMoreElements()) {
            try {
                register(((ZipEntry)en.nextElement()).getName());
            } catch (Exception e) {
            	//ignore
            }
        }
    }

    private void register(String path) {
        String[] parsed = parse(path);
        check(parsed[0]);
        HashSet<String> set = map.get(parsed[0]);
        if(!"/".equals(parsed[1])) set.add(parsed[1]);
    }

    private String[] parse(String path) {
        String q = path;
        if(path.endsWith("/")) q = q.substring(0, path.length() - 1);
        int i = q.lastIndexOf('/');
        String root = (i < 0) ? "" : path.substring(0, i);
        String name = (i < 0) ? path : path.substring(i + 1);
        return new String[]{root, name};
    }

    private void check(String path) {
        if(map.get(path) != null) return;
        map.put(path, new HashSet<String>());
        String[] parsed = parse(path);
        check(parsed[0]);
        if("".equals(parsed[1])) return;
        HashSet<String> set = map.get(parsed[0]);
        set.add(parsed[1] + "/");
    }

    public String[] getChildren(String path) {
        HashSet<String> set = map.get(path);
        return (set == null) ? new String[0] : set.toArray(new String[0]);
    }

    public long getSize(String path) {
        try {
            return jar.getEntry(path).getSize();
        } catch (Exception e) {
            return 0;
        } 
    }

    public String getContent(String path) {
        int size = 1024;
        byte b[] = new byte[size];
        StringBuffer sb = new StringBuffer();
        int length = 0;
        try {
            InputStream is = jar.getInputStream(jar.getEntry(path));
            BufferedInputStream bs = new BufferedInputStream(is);
            while((length = bs.available()) > 0) {
                if(length > size) length = size;
                length = bs.read(b, 0, length);
                if(length < 0) break;
                sb.append(new String(b, 0, length));
            }
            return sb.toString();
        } catch (Exception e) {
            return "";
        }
    }

    public boolean isTextEntry(String path, int length) {
        String b = getContent(path);
        b = (b == null || b.length() < length) ? b : b.substring(length);
        return FileUtil.isText(b);
    }

    boolean hasFolder(String path) {
        return map.get(path) != null;
    }

    boolean hasFile(String path) {
        if(path == null) return false;
        int i = path.lastIndexOf('/');
        String p = (i < 0) ? "" : path.substring(0, i);
        String n = path.substring(i + 1);
        HashSet set = (HashSet)map.get(p);
        return set != null && set.contains(n);
    }

    public LFileObject getFileObject(String alias, String relpath) {
        return new LFileObjectJarImpl(this, alias, relpath);
    }

    public boolean isModified() {
        if(timeStamp == -1) return true;
        try {
        	File f = new File(location);
            return (timeStamp != f.lastModified() || size != f.length());
        } catch (Exception e) {
            return true;
        }
    }

    public void invalidate() {
        if(jar != null) {
            try {
            	jar.close();
            } catch (Exception e) {
            	//ignore
            }
        }
        jar = null;
        map.clear();
        timeStamp = -1;
        size = -1;
    }

    public String getTempLocation() {
        return templocation;
    }

}

class LFileObjectJarImpl implements LFileObject {
    private JarAccess access = null;
    private String aliaspath = null;
    private String relpath = null;

    public LFileObjectJarImpl(JarAccess access, String alias, String relpath) {
        this.access = access;
        aliaspath = relpath.length() == 0 ? alias : alias + '/' + relpath;
        this.relpath = relpath;
    }

    public String getName() {
        return relpath.substring(relpath.lastIndexOf('/') + 1);
    }

    public boolean exists() {
        return access.hasFolder(relpath) || access.hasFile(relpath);
    }

    public boolean isDirectory() {
        return access.hasFolder(relpath);
    }

    public boolean isFile() {
        return access.hasFile(relpath);
    }

    public long lastModified() {
        return 0;
    }

    public String getPath() {
        return aliaspath;
    }

    public boolean canWrite() {
        return false;
    }

    public String read() {
        return access.getContent(relpath);
    }

    public void write(String s) {}

    public String[] listFiles() {
        String[] r = access.getChildren(relpath);
        String rp = getPath();
        for (int i = 0; i < r.length; i++) {
            if(r[i].endsWith("/")) r[i] = r[i].substring(0, r[i].length() - 1);
            r[i] = rp + "/" + r[i];
        }
        return r;
    }

    public boolean mkdirs() {
        return false;
    }

    public boolean delete() {
        return false;
    }

}

