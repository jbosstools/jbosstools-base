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
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import org.eclipse.core.resources.IProject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
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

	List<String> errors = new ArrayList<String>();

	public JarAccess() {}

	public List<String> getErrors() {
		return errors;
	}

	public void setLocation(String location) {
		this.location = location;
		validate();
	}
    
	public synchronized void lockJar() {
		jarLock++;
	}

	public String getLocation() {
		return location;
	}

	public synchronized boolean isLoaded() {
		return (exists || loading);
	}

	public synchronized void validate() {
		if (isLoaded()) return;
		loading = true;
		templocation = null;
		try {
			int ind = location.indexOf(":/"); //$NON-NLS-1$
			if (ind != 1 && ind != -1) {
				int extI = location.lastIndexOf('.');
				String ext = extI >= 0 ? location.substring(extI) : ".jar"; //$NON-NLS-1$
				File f = File.createTempFile("efs_", ext); //$NON-NLS-1$
				f.deleteOnExit();
				InputStream i = new java.net.URL(location).openConnection().getInputStream();
				FileOutputStream o = new FileOutputStream(f);
				FileUtil.copy(i, o);
				timeStamp = -1;
				size = -1;
				templocation = f.getCanonicalPath();
			} else {
				File nf = new File(location);
				templocation = nf.getCanonicalPath();
				timeStamp = nf.lastModified();
				size = nf.length();
			}
			init();
			exists = true;
		} catch (IOException e) {
			timeStamp = -1;
			size = -1;
			exists = false;
			return;
		} finally {
			loading = false;
		}
	}

	private void init() throws IOException  {
		ZipFile jar = getZipFile();
		map.clear();
		fileEntries.clear();
		map.put("", new HashSet<String>()); //$NON-NLS-1$
		try {
			if(jar == null) return;
			Enumeration<?> en = jar.entries();
			while(en.hasMoreElements()) {
					ZipEntry entry = (ZipEntry)en.nextElement();
					String name = entry.getName();
					if(name != null && name.endsWith(".class")) {
						//Ignore .class entries. They are handled by JDT.
						continue;
					}
					if(name != null && !name.endsWith(XModelObjectConstants.SEPARATOR) && entry.getSize() > 0) {
						fileEntries.put(name, Long.valueOf(entry.getSize()));
					}
					register(name);
			}
		} finally {
			unlockJar();
		}
	}

	private ZipFile getZipFile() throws IOException {
		synchronized (this) {
			lockJar();
			if(!new File(templocation).isFile()) return null;
			if(jar != null) return jar;
			return jar = new ZipFile(templocation);
		}
	}

	public synchronized void unlockJar() {
		jarLock--;
		if(jarLock > 0 || jar == null) return;
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

	private void register(String path) {
		String[] parsed = parse(path);
		check(parsed[0]);
		HashSet<String> set = map.get(parsed[0]);
		if (!XModelObjectConstants.SEPARATOR.equals(parsed[1]))
			set.add(parsed[1]);
	}

	private String[] parse(String path) {
		String q = path;
		if (path.endsWith(XModelObjectConstants.SEPARATOR))
			q = q.substring(0, path.length() - 1);
		int i = q.lastIndexOf('/');
		String root = (i < 0) ? "" : path.substring(0, i); //$NON-NLS-1$
		String name = (i < 0) ? path : path.substring(i + 1);
		return new String[] { root, name };
	}

	private void check(String path) {
		if (map.get(path) != null)
			return;
		map.put(path, new HashSet<String>());
		String[] parsed = parse(path);
		check(parsed[0]);
		if ("".equals(parsed[1])) //$NON-NLS-1$
			return;
		HashSet<String> set = map.get(parsed[0]);
		set.add(parsed[1] + XModelObjectConstants.SEPARATOR);
	}

	public synchronized String[] getChildren(String path) {
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
		} catch (IOException e) {
			String error = "JarAccess: cannot load zip file for location " + templocation; //$NON-NLS-1$
			errors.add(error);
			ModelPlugin.getDefault().logError(error);
		}
		if(jar == null) {
			unlockJar();
			return ""; //$NON-NLS-1$
		}
		int length = 0;
		BufferedInputStream bs = null;
		String encoding = null;
		boolean first = true;
		try {
			ZipEntry entry = jar.getEntry(path);
			if(entry == null && fileEntries.containsKey("/" + path)) {
				entry = jar.getEntry("/" + path);
			}
			if(entry == null) {
				String error = "JarAccess: cannot obtain entry for path '" + path + "' from jar '" + location + "'.";  //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
				errors.add(error);
				ModelPlugin.getDefault().logError(error);
				return ""; //$NON-NLS-1$
			}
			
			InputStream is = jar.getInputStream(entry);
			bs = new BufferedInputStream(is);
			while ((length = bs.available()) > 0) {
				if (length > size)
					length = size;
				length = bs.read(b, 0, length);
				if (length < 0)
					break;
				if(first) {
					first = false;
					encoding = FileUtil.getEncoding(b);
				}
				if(encoding != null) {
					sb.append(new String(b, 0, length, encoding));
				} else {
					sb.append(new String(b, 0, length));
				}
			}
			return sb.toString();
		} catch (IOException e) {
			errors.add(e.getClass().getName() + " occurs when reading " + jar.getName() + " : " + e.getMessage());  //$NON-NLS-1$//$NON-NLS-2$
			ModelPlugin.getPluginLog().logError("Exception occurs when reading " + jar.getName(), e); //$NON-NLS-1$
			return ""; //$NON-NLS-1$
		} finally {
			unlockJar();
			if(bs!=null) {
				try {
					bs.close();
				} catch (IOException e) {
					//ignore
				}
			}
		}
	}

	public boolean isTextEntry(String path, int length) {
		String b = getContent(path);
		b = (b == null || b.length() < length) ? b : b.substring(length);
		return FileUtil.isText(b);
	}

	synchronized boolean hasFolder(String path) {
		return map.containsKey(path);
	}

	synchronized boolean hasFile(String path) {
		if (path == null)
			return false;
		int i = path.lastIndexOf('/');
		String p = (i < 0) ? "" : path.substring(0, i); //$NON-NLS-1$
		String n = path.substring(i + 1);
		Set<String> set = map.get(p);
		return set != null && set.contains(n);
	}

	public LFileObject getFileObject(String alias, String relpath) {
		return new LFileObjectJarImpl(this, alias, relpath);
	}

	public boolean isModified() {
		if (timeStamp == -1) {
			return true;
		}
		File f = new File(location);
		return (timeStamp != f.lastModified() || size != f.length());
	}

	public synchronized void invalidate() {
		exists = false;
		map.clear();
		timeStamp = -1;
		size = -1;
	}

	public String getTempLocation() {
		return templocation;
	}

	JarSystemImpl main = null;
	Set<JarSystemImpl> slaves = new HashSet<JarSystemImpl>();

	public JarSystemImpl getMain() {
		IProject p = EclipseResourceUtil.getProject(main);
		if(p == null || !p.isAccessible() || !main.isActive()) {
			main = null;
			synchronized(slaves) {
				Iterator<JarSystemImpl> it = slaves.iterator();
				while(it.hasNext()) {
					JarSystemImpl s = it.next();
					p = EclipseResourceUtil.getProject(s);
					if(p == null || !p.isAccessible() || !s.isActive()) {
						it.remove();
					} else if(main == null) {
						main = s;
						it.remove();
					}					
				}			
			}
            if(main != null) main.jarUpdated();
            JarSystemImpl[] ss = getSlaves();
            for (JarSystemImpl s: ss) s.jarUpdated();
		}
		return main;
	}

	public void setMain(JarSystemImpl main) {
		this.main = main;
	}

	public JarSystemImpl[] getSlaves() {
		synchronized(slaves) {
			return slaves.toArray(new JarSystemImpl[slaves.size()]);
		}
	}

	public void addSlave(JarSystemImpl s) {
		if(main == null) {
			main = s;
		} else {
			synchronized(slaves) {
				slaves.add(s);
			}
		}
	}

	public boolean isSlave(JarSystemImpl s) {
		return slaves.contains(s);
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
			if (r[i].endsWith(XModelObjectConstants.SEPARATOR))
				r[i] = r[i].substring(0, r[i].length() - 1);
			r[i] = rp + XModelObjectConstants.SEPARATOR + r[i];
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
