/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.model.filesystems.impl;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ElementChangedEvent;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IElementChangedListener;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaElementDelta;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.XModelObjectUtil;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class Libs implements IElementChangedListener {
	protected FileSystemsImpl object;
	protected List<String> paths = null;
	Map<IPath, String> paths2 = new HashMap<IPath, String>();
	Set<String> projects = new HashSet<String>();

	LibraryNames libraryNames = new LibraryNames();

	List<LibsListener> listeners = new ArrayList<LibsListener>();
	
	public Libs(FileSystemsImpl object) {
		this.object = object;
	}

	public void init() {
		JavaCore.addElementChangedListener(this);
	}

	private IProject getProjectResource() {
		return EclipseResourceUtil.getProject(object);
	}

	public XModelObject getLibrary(String path) {
		String libName = libraryNames.getName(path);
		if(libName == null) {
			//compatibility to old code.
			libName = LIB_PREFIX + new File(path).getName();
		}
		return object.getChildByPath(libName);
	}

	public XModelObject getLibrary(File f) {
		XModelObject result = null;
		if(f.exists()) {
			String path = f.getAbsolutePath().replace('\\', '/');
			result = getLibrary(path);
		}
		return result;
	}

	public boolean update() {
		int cpv = classpathVersion;
		boolean result = hasToUpdatePaths() && updatePaths(getNewPaths(), cpv);
		if(result) fire();
		if(paths == null && result) return true;
	
		if(paths != null) updateFileSystems(paths);
		fsVersion = pathsVersion;

		return result;
	}

	public void requestForUpdate() {
		classpathVersion++;
	}

	synchronized boolean hasToUpdatePaths() {
		return (classpathVersion > pathsVersion);
	}

	private List<String> getNewPaths() {
		List<String> result = null;
		try {
			result = EclipseResourceUtil.getAllVisibleLibraries(getProjectResource());
			List<String> jre = EclipseResourceUtil.getJREClassPath(getProjectResource());
			if(jre != null) result.removeAll(jre);
			updateProjects();
		} catch (CoreException e) {
			ModelPlugin.getDefault().logError(e);
		}
		return result;
	}

	private void updateProjects() throws JavaModelException {
		Set<String> result = new HashSet<String>();
		IJavaProject javaProject = EclipseResourceUtil.getJavaProject(getProjectResource());
		if(javaProject != null) {
			result.add(getProjectResource().getName());
			IClasspathEntry[] es = javaProject.getResolvedClasspath(true);
			for (int i = 0; i < es.length; i++) {
				if(es[i].getEntryKind() == IClasspathEntry.CPE_PROJECT) {
					IProject p = ResourcesPlugin.getWorkspace().getRoot().getProject(es[i].getPath().lastSegment());
					if(p == null || !p.isAccessible()) continue;
					result.add(p.getName());
				}
			}
		}
		projects = result;
	}

	private synchronized boolean updatePaths(List<String> newPaths, int cpv) {
		if(cpv <= pathsVersion) {
			return false;
		}
		pathsVersion = cpv;
		if(paths == null && newPaths == null) return false;
		if((newPaths != null && paths != null) && (paths.size() == newPaths.size())) {
			boolean b = false;
			for (int i = 0; i < paths.size() && !b; i++) {
				if(!paths.get(i).equals(newPaths.get(i))) b = true;
			}
			if(!b) return false;
		}
		paths = newPaths;
		createMap();
		return true;
	}
	
	public static String LIB_PREFIX = "lib-"; //$NON-NLS-1$

	void updateFileSystems(List<String> paths) {
		if(fsVersion >= pathsVersion) {
			return;
		}
		
		Set<String> oldPaths = libraryNames.getPaths();
		for (String p: oldPaths) {
			if(!paths.contains(p)) {
				String n = libraryNames.getName(p);
				XModelObject o = object.getChildByPath(n);
				if(o != null) {
					o.removeFromParent();
				}
				libraryNames.removePath(p);
			}
		}
		
		XModelObject[] fs = object.getChildren();
		Set<XModelObject> fss = new HashSet<XModelObject>();
		for (int i = 0; i < fs.length; i++) {
			if(fs[i].getAttributeValue(XModelObjectConstants.ATTR_NAME).startsWith(LIB_PREFIX)) {
				fss.add(fs[i]);
			}
		}
		
		if(paths != null) for (int i = 0; i < paths.size(); i++) {
			String path = paths.get(i);
			boolean isJar = EclipseResourceUtil.isJar(path);
			String libEntity = isJar ? "FileSystemJar" : "FileSystemFolder"; //$NON-NLS-1$ //$NON-NLS-2$
			String fileName = new File(path).getName();
			if(isJar && EclipseResourceUtil.SYSTEM_JAR_SET.contains(fileName)) continue;
			String jsname = libraryNames.getName(path);
			if(jsname == null) {
				jsname = LIB_PREFIX + fileName;
				int q = 0;
				while(libraryNames.hasName(jsname)) {
					jsname = LIB_PREFIX + fileName + "-" + (++q); //$NON-NLS-1$
				}				
			}
			XModelObject o = object.getChildByPath(jsname);
			if(o != null) {
				fss.remove(o);
			} else {
				o = object.getModel().createModelObject(libEntity, null);
				o.setAttributeValue(XModelObjectConstants.ATTR_NAME, jsname); 
				o.setAttributeValue(XModelObjectConstants.ATTR_NAME_LOCATION, path);
				o.set(FileSystemsLoader.IS_ADDED_TO_CLASSPATH, XModelObjectConstants.TRUE);
				object.addChild(o);
//				object.setModified(true);
			}
			libraryNames.put(path, jsname);
		}
		
		for (XModelObject o: fss) {
			String path = XModelObjectUtil.expand(o.getAttributeValue(XModelObjectConstants.ATTR_NAME_LOCATION), o.getModel(), null);
			if(XModelObjectConstants.TRUE.equals(o.get(FileSystemsLoader.IS_ADDED_TO_CLASSPATH))) {
				o.removeFromParent(); 
			} else if(!new File(path).exists()) {
				o.removeFromParent();
			}			
		}
	}

	public List<String> getPaths() {
		return paths;
	}

	public Map<IPath, String> getPathsAsMap() {
		return paths2;
	}
	
	private void createMap() {
		paths2.clear();
		if(paths != null) {
			for (String p : paths) {
				paths2.put(new Path(p), p);
			}
		}
	}

	public synchronized void addListener(LibsListener listener) {
		listeners.add(listener);
	}

	public synchronized void removeListener(LibsListener listener) {
		listeners.remove(listener);
	}

	void fire() {
		LibsListener[] ls = null;
		synchronized(this) {
			ls = listeners.toArray(new LibsListener[0]);
		}
		if(ls != null) for (LibsListener listener: ls) {
			listener.pathsChanged(paths);
		}
	}

	int classpathVersion = 0;
	int pathsVersion = -1;
	int fsVersion = -1;

	public void elementChanged(ElementChangedEvent event) {
		IProject project = getProjectResource();
		if(project == null || !project.exists()) {
			JavaCore.removeElementChangedListener(this);
			return;
		}

		for (IJavaElementDelta dc: event.getDelta().getAffectedChildren()) {
			if(dc.getElement() instanceof IJavaProject && (isReleventProject(((IJavaProject)dc.getElement()).getProject()))) {
				int f = dc.getFlags();
				if((f & (IJavaElementDelta.F_CLASSPATH_CHANGED 
					| IJavaElementDelta.F_RESOLVED_CLASSPATH_CHANGED)) != 0) {
					requestForUpdate();
					return;
				} else {
					for (IJavaElementDelta d1: dc.getAffectedChildren()) {
						IJavaElement e = d1.getElement();
						if(d1.getKind() == IJavaElementDelta.ADDED) {
							requestForUpdate();
							return;
						}
					}
				}
			}
		}
	}

	private boolean isReleventProject(IProject p) {
		return projects.contains(p.getName());
	}

}

class LibraryNames {
	private Map<String, String> pathToName = new HashMap<String, String>();
	private Map<String, String> nameToPath = new HashMap<String, String>();

	public synchronized void put(String path, String name) {
		pathToName.put(path, name);
		nameToPath.put(name, path);
	}

	public synchronized void removePath(String path) {
		String name = pathToName.remove(path);
		if(name != null) {
			nameToPath.remove(name);
		}
	}

	public String getName(String path) {
		return pathToName.get(path);
	}

	public String getPath(String name) {
		return nameToPath.get(name);
	}

	public boolean hasName(String name) {
		return nameToPath.containsKey(name);
	}

	public synchronized Set<String> getPaths() {
		return new HashSet<String>(pathToName.keySet());
	}
}
