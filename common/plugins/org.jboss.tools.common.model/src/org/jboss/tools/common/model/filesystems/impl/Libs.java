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
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ElementChangedEvent;
import org.eclipse.jdt.core.IElementChangedListener;
import org.eclipse.jdt.core.IJavaElementDelta;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
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

	List<LibsListener> listeners = new ArrayList<LibsListener>();
	
	public Libs(FileSystemsImpl object) {
		this.object = object;
	}

	public void init() {
		JavaCore.addElementChangedListener(this);
	}

	private IProject getProjectResource() {
		try {
			EclipseResourceUtil.getProject(object);
		} catch (NullPointerException e) {
			e.printStackTrace();
		}
		return EclipseResourceUtil.getProject(object);
	}

	public boolean update() {
		boolean result = updatePaths();
		if(result) fire();
		if(paths == null && result) return true;
	
		updateFileSystems(paths);
		fsVersion = pathsVersion;

		return result;
	}

	synchronized boolean updatePaths() {
		if(classpathpVersion <= pathsVersion) {
			return false;
		}
		pathsVersion = classpathpVersion;
		List<String> newPaths = null;
		try {
			newPaths = EclipseResourceUtil.getClassPath(getProjectResource());
			List<String> jre = EclipseResourceUtil.getJREClassPath(getProjectResource());
			if(jre != null) newPaths.removeAll(jre);
		} catch (CoreException e) {
			ModelPlugin.getDefault().logError(e);
		} catch(IOException e) {
			ModelPlugin.getDefault().logError(e);			
		}
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

	void updateFileSystems(List<String> paths) {
		if(fsVersion >= pathsVersion) {
			return;
		}
		XModelObject[] fs = object.getChildren("FileSystemJar"); //$NON-NLS-1$
		Set<XModelObject> fss = new HashSet<XModelObject>();
		for (int i = 0; i < fs.length; i++) fss.add(fs[i]);
		
		if(paths != null) for (int i = 0; i < paths.size(); i++) {
			String path = paths.get(i);
			if(!EclipseResourceUtil.isJar(path)) continue;
			String fileName = new File(path).getName();
			if(EclipseResourceUtil.SYSTEM_JAR_SET.contains(fileName)) continue;
			String jsname = "lib-" + fileName; //$NON-NLS-1$
			XModelObject o = object.getChildByPath(jsname);
			if(o != null) {
				fss.remove(o);
			} else {
				o = object.getModel().createModelObject("FileSystemJar", null); //$NON-NLS-1$
				o.setAttributeValue(XModelObjectConstants.ATTR_NAME, jsname); 
				o.setAttributeValue(XModelObjectConstants.ATTR_NAME_LOCATION, path);
				o.set(FileSystemsLoader.IS_ADDED_TO_CLASSPATH, XModelObjectConstants.TRUE);
				object.addChild(o);
//				object.setModified(true);
			}			
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

	int classpathpVersion = 0;
	int pathsVersion = -1;
	int fsVersion = -1;

	public void elementChanged(ElementChangedEvent event) {
		IProject project = getProjectResource();
		if(project == null || !project.exists()) {
			JavaCore.removeElementChangedListener(this);
			return;
		}

		IJavaElementDelta d = event.getDelta();
		IJavaElementDelta[] ds = d.getAffectedChildren();
		IJavaElementDelta p = null;

		for (IJavaElementDelta dc: ds) {
			if(dc.getElement() instanceof IJavaProject && ((IJavaProject)dc.getElement()).getProject() == project) {
				p = dc;
			}
		}
		if(p == null) return;
		int f = p.getFlags();
		if((f & (IJavaElementDelta.F_CLASSPATH_CHANGED 
			| IJavaElementDelta.F_RESOLVED_CLASSPATH_CHANGED)) != 0) {
			classpathpVersion++;
		}		
	}

}
