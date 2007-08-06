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
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.util.FileUtil;

public class JarAccess {
	private String location = null;
	private String templocation = null;

	private ZipFile jar = null;
	int jarLock = 0;

	private Map<String,HashSet<String>> map = new HashMap<String,HashSet<String>>();
	private Map<String,Long> fileEntries = new HashMap<String,Long>();

	private boolean loading = false;
	private boolean exists = false;
	private long timeStamp = -1;
	private long size = -1;

	public JarAccess() {}

	public void setLocation(String location) {
		this.location = location;
		validate();
	}
    
	public void lockJar() {
		jarLock++;
	}

	public String getLocation() {
		return location;
	}

	public boolean isLoaded() {
		return (exists || loading);
	}

	public void validate() {
		if (isLoaded()) return;
		loading = true;
		templocation = null;
		try {
			int ind = location.indexOf(":/");
			if (ind != 1 && ind != -1) {
				File f = File.createTempFile("efs_", ".jar");
				f.deleteOnExit();
				InputStream i = new java.net.URL(location).openConnection().getInputStream();
				FileOutputStream o = new FileOutputStream(f);
				FileUtil.copy(i, o);
				timeStamp = -1;
				size = -1;
				templocation = f.getCanonicalPath();
			} else {
				File nf = new File(location);
//				FileUtil.copyFile(nf, f, true);
				templocation = nf.getCanonicalPath();
				timeStamp = nf.lastModified();
				size = nf.length();
			}
			init();
			exists = true;
		} catch (Exception e) {
			timeStamp = -1;
			size = -1;
			exists = false;
			return;
		} finally {
			loading = false;
		}
	}

	private void init() throws Exception {
		ZipFile jar = getZipFile();
		map.clear();
		fileEntries.clear();
		map.put("", new HashSet<String>());
		try {
			if(jar == null) return;
			Enumeration<?> en = jar.entries();
			while(en.hasMoreElements()) {
				try {
					ZipEntry entry = (ZipEntry)en.nextElement();
					String name = entry.getName();
					if(name != null && !name.endsWith("/") && entry.getSize() > 0) {
						fileEntries.put(name, new Long(entry.getSize()));
					}
					register(name);
				} catch (Exception e) {
					ModelPlugin.getPluginLog().logError(e);
				}
			}
		} finally {
			unlockJar();
		}
	}

	private ZipFile getZipFile() throws IOException {
		synchronized (this) {
			lockJar();
			if(jar != null) return jar;
			return jar = new ZipFile(templocation);
		}
	}

	public void unlockJar() {
		jarLock--;
		if(jarLock > 0 || jar == null) return;
		synchronized (this) {
			if(jar != null && jarLock == 0) {
				try {
					jar.close();
				} catch (IOException e) {
            		//ignore
				} finally {
					jar = null;
				}
			}
		}
	}

	private void register(String path) {
		String[] parsed = parse(path);
		check(parsed[0]);
		HashSet<String> set = map.get(parsed[0]);
		if (!"/".equals(parsed[1]))
			set.add(parsed[1]);
	}

	private String[] parse(String path) {
		String q = path;
		if (path.endsWith("/"))
			q = q.substring(0, path.length() - 1);
		int i = q.lastIndexOf('/');
		String root = (i < 0) ? "" : path.substring(0, i);
		String name = (i < 0) ? path : path.substring(i + 1);
		return new String[] { root, name };
	}

	private void check(String path) {
		if (map.get(path) != null)
			return;
		map.put(path, new HashSet<String>());
		String[] parsed = parse(path);
		check(parsed[0]);
		if ("".equals(parsed[1]))
			return;
		HashSet<String> set = map.get(parsed[0]);
		set.add(parsed[1] + "/");
	}

	public String[] getChildren(String path) {
		HashSet<String> set = map.get(path);
		return (set == null) ? new String[0] : set.toArray(new String[0]);
	}

	public long getSize(String path) {
		Long s = fileEntries.get(path);
		return s == null ? 0 : s.longValue();
	}

	public String getContent(String path) {
		int size = 1024;
		byte b[] = new byte[size];
		StringBuffer sb = new StringBuffer();
		ZipFile jar = null;
		try {
			jar = getZipFile();
		} catch (Exception e) {
			unlockJar();
			return "";
		}
		int length = 0;
		try {
			InputStream is = jar.getInputStream(jar.getEntry(path));
			BufferedInputStream bs = new BufferedInputStream(is);
			while ((length = bs.available()) > 0) {
				if (length > size)
					length = size;
				length = bs.read(b, 0, length);
				if (length < 0)
					break;
				sb.append(new String(b, 0, length));
			}
			return sb.toString();
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
			return "";
		} finally {
			unlockJar();
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
		if (path == null)
			return false;
		int i = path.lastIndexOf('/');
		String p = (i < 0) ? "" : path.substring(0, i);
		String n = path.substring(i + 1);
		Set<String> set = map.get(p);
		return set != null && set.contains(n);
	}

	public LFileObject getFileObject(String alias, String relpath) {
		return new LFileObjectJarImpl(this, alias, relpath);
	}

	public boolean isModified() {
		if (timeStamp == -1)
			return true;
		try {
			File f = new File(location);
			return (timeStamp != f.lastModified() || size != f.length());
		} catch (Exception e) {
			return true;
		}
	}

	public void invalidate() {
		exists = false;
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

	public void write(String s) {
	}

	public String[] listFiles() {
		String[] r = access.getChildren(relpath);
		String rp = getPath();
		for (int i = 0; i < r.length; i++) {
			if (r[i].endsWith("/"))
				r[i] = r[i].substring(0, r[i].length() - 1);
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
