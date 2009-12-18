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
package org.jboss.tools.common.resref.core;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.eclipse.core.runtime.preferences.InstanceScope;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.XModelObjectUtil;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public abstract class ResourceReferenceList {
	
	//Fix for JBIDE-2979
	private static final String CODING = "UTF-16";//$NON-NLS-1$
	
	ResourceReferenceListListener[] listeners = new ResourceReferenceListListener[0];
	
	protected abstract QualifiedName getPropertyName();
	
	public void addChangeListener(ResourceReferenceListListener listener) {
		int i = getListenerIndex(listener);
		if(i >= 0) return;
		ResourceReferenceListListener[] ls = new ResourceReferenceListListener[listeners.length + 1];
		System.arraycopy(listeners, 0, ls, 0, listeners.length);
		ls[listeners.length] = listener;
		listeners = ls;
	}
	
	public void removeChangeListener(ResourceReferenceListListener listener) {
		int i = getListenerIndex(listener);
		if(i < 0) return;
		ResourceReferenceListListener[] ls = new ResourceReferenceListListener[listeners.length - 1];
		if(i > 0) System.arraycopy(listeners, 0, ls, 0, i);
		if(i < ls.length) System.arraycopy(listeners, i + 1, ls, i, ls.length - i);
		listeners = ls;
	}

	private int getListenerIndex(ResourceReferenceListListener listener) {
		for (int i = 0; i < listeners.length; i++) {
			if(listeners[i] == listener) return i;
		}
		return -1;
	}
	
	/**
	 * Returns all resources for current file
	 * @param file
	 * @return
	 */
	public ResourceReference[] getAllResources(IFile file) {
		Set<String> locations = new HashSet<String>();
		List<ResourceReference> css = new ArrayList<ResourceReference>();
		
		if(file.getProject() != null) {
			String[] dcss = getDeclaredResources(file.getProject());
			for (int i = 0; i < dcss.length; i++) {
				ResourceReference ref = new ResourceReference(dcss[i], ResourceReference.PROJECT_SCOPE);
				locations.add(dcss[i]);
				css.add(ref);
			}
		}
		IResource parent = file.getParent();
		int depth = 0;
		while(parent instanceof IFolder) {
			String[] dcss = getDeclaredResources(parent);
			for (int i = 0; i < dcss.length; i++) {
				if(locations.contains(dcss[i])) continue;
				ResourceReference ref = new ResourceReference(dcss[i], ResourceReference.FOLDER_SCOPE);
				ref.setDepth(depth);
				locations.add(dcss[i]);
				css.add(ref);
			}
			parent = parent.getParent();
			depth++;
		}
		String[] dcss = getDeclaredResources(file);
		for (int i = 0; i < dcss.length; i++) {
			if(locations.contains(dcss[i])) continue;
			ResourceReference ref = new ResourceReference(dcss[i], ResourceReference.FILE_SCOPE);
			locations.add(dcss[i]);
			css.add(ref);
		}
		return css.toArray(new ResourceReference[0]);		
	}
	
	private String[] getDeclaredResources(IResource resource) {
	    /*
	     * Property value
	     */
	    String s = null;
	    /*
	     * https://jira.jboss.org/jira/browse/JBIDE-3211
	     * Reading preferences from project scope from .settings
	     */
	    IScopeContext projectScope = new ProjectScope(resource.getProject());
	    IEclipsePreferences root = projectScope
	    	.getNode(ResourceReferencePlugin.PLUGIN_ID);
	    if (root != null) {
		/*
		 * Create subnode for file/folder/project
		 */
		IPath path = resource.getProjectRelativePath();
		String nodeString = path.toString();
		Preferences node = root.node(nodeString);
		if (null != node) {
		    s = node.get(getPropertyName().getLocalName(), ""); //$NON-NLS-1$
		}
	    }
		if (s == null || s.length() == 0) {
			/*
			 * Old preferences format property value.
			 */
			String old = null;
			try {
				old = resource.getPersistentProperty(getPropertyName());
			} catch (CoreException e) {
				/*
				 * Ignore, there is no properties for this resource.
				 */
			}
			if (old == null || old.length() == 0) {
				return new String[0];
			} else {
				/*
				 * If there is property stored in the old preferences format
				 * return this value.
				 */
				return decodeResourceString(old);
			}
		} else {
			return decodeResourceString(s);
		}
	}
	
	//Fix for JBIDE-2979
	private static String[] decodeResourceString(String resource) {
		String[] results = XModelObjectUtil.asStringArray(resource);
		String[] returnValues = new String [results.length]; 
		try {
			for (int i=0;i<results.length;i++) {	
					returnValues[i] = URLDecoder.decode(results[i], CODING);	
			}
		} catch (UnsupportedEncodingException e) {
			ResourceReferencePlugin.getPluginLog().logError(e);
			//in case of exception we return undecoded values
			return results;
		}
		return returnValues;
	}
	
	
	public void setAllResources(IFile file, ResourceReference[] entries) {
	    	IResource changed = null;
		boolean b = setDeclaredResources(file, entries, ResourceReference.FILE_SCOPE, 0);
		if(b) changed = file;
		IResource parent = file.getParent();
		int depth = 0;
		while(parent instanceof IFolder) {
			b = setDeclaredResources(parent, entries, ResourceReference.FOLDER_SCOPE, depth);
			if(b) {
			    changed = parent;
			}
			parent = parent.getParent();
			depth++;
		}
		if(file.getProject() != null) {
			int scope = ResourceReference.PROJECT_SCOPE;
			if(file.getParent() == file.getProject()) scope = 10;
			b = setDeclaredResources(file.getProject(), entries, scope, 0);
			if(b) {
			    changed = file.getProject();
			}
		}
		if(changed != null) fire(changed.getFullPath());
	}

	private boolean setDeclaredResources(IResource resource, ResourceReference[] entries, int scope, int depth) {
	    /*
	     * https://jira.jboss.org/jira/browse/JBIDE-3211
	     * Storing project preferences to project scope in .settings
	     */
	    IScopeContext projectScope = new ProjectScope(resource.getProject());
	    IEclipsePreferences root = projectScope
	    	.getNode(ResourceReferencePlugin.PLUGIN_ID);
	    if (root != null) {
		/*
		 * Get subnode for file/folder/project
		 */
		IPath path = resource.getProjectRelativePath();
		String nodeString = path.toString();
		Preferences node = root.node(nodeString);
		String oldValue = node.get(getPropertyName().getLocalName(), ""); //$NON-NLS-1$
		String newValue = encodeDeclaredResources(entries, scope, depth);
		if (oldValue.equals(newValue)) {
		    return false;
		}
		node.put(getPropertyName().getLocalName(), newValue);
		try {
		    root.flush();
		} catch (BackingStoreException e) {
		    ResourceReferencePlugin.getPluginLog().logError(e);
		}
	    }
	    return true;
	}
	
	private String encodeDeclaredResources(ResourceReference[] entries, int scope, int depth) {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < entries.length; i++) {
			int s = entries[i].getScope();
			if(scope < 10 && s != scope) continue;
			if(scope == 10 && s == ResourceReference.FILE_SCOPE) continue;
			if(scope == ResourceReference.FOLDER_SCOPE && entries[i].getDepth() != depth) continue;
			if(sb.length() > 0) 
				sb.append(';');
			try {
				sb.append(URLEncoder.encode(entries[i].getLocationAndProperties(),CODING));
			} catch (UnsupportedEncodingException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return sb.toString();
	}
	
	void fire(IPath path) {
		ResourceReferenceListListener[] ls = listeners;
		for (int i = 0; i < ls.length; i++) {
			IPath listenedPath = ls[i].getPath();
			if(listenedPath != null && path.isPrefixOf(listenedPath)) {
				ls[i].changed(this);
			}
		}
	}
	
	/*
	 * Handle opened external files
	 */
	TreeMap allExternalResources = null;
	
	private TreeMap getAllExternalResources() {
		if (allExternalResources == null) {
			allExternalResources = new TreeMap();
			/*
			 * Property value
			 */
			String s = null;
			/*
			 * https://jira.jboss.org/jira/browse/JBIDE-3211 
			 * Reading global preferences from instance scope.
			 */
			IScopeContext instanceContext = new InstanceScope();
			Preferences root = instanceContext.getNode(ModelPlugin.PLUGIN_ID);
			if (null != root) {
				Preferences node = root.node(ResourceReferencePlugin.PLUGIN_ID);
				s = node.get(getPropertyName().getLocalName(), ""); //$NON-NLS-1$
			}
			/*
			 * If there is property stored in the old preferences format and
			 * there are no other properties old value will be returned
			 */
			if (s != null && s.length() > 0) {
				parseExternalResources(s);
			} else {
				/*
				 * Old preferences format property value.
				 */
				String old = null;
				try {
					old = ModelPlugin.getWorkspace().getRoot()
							.getPersistentProperty(getPropertyName());
				} catch (CoreException e) {
					/*
					 * Ignore, there is no properties for this resource.
					 */
				}
				if (old != null && old.length() > 0) {
					parseExternalResources(old);
				}
			}
		}
		return allExternalResources;
	}
	
	private void parseExternalResources(String s) {
		StringTokenizer st = new StringTokenizer(s, "#"); //$NON-NLS-1$
		while(st.hasMoreTokens()) {
			String t = st.nextToken();
			int e = t.indexOf('=');
			String path = t.substring(0, e);
			String list = t.substring(e + 1);
			if(new File(path).exists()) {
			    allExternalResources.put(path, list);
			}
		}
	}

	private void setAllExternalResources() {
		StringBuffer sb = new StringBuffer();
		Iterator it = allExternalResources.keySet().iterator();
		while (it.hasNext()) {
			String path = it.next().toString();
			String list = (String) allExternalResources.get(path);
			if (path != null && list != null && new File(path).exists()) {
				if (sb.length() > 0)
					sb.append('#');
				sb.append(path).append('=').append(list);
			}
		}
		/*
		 * https://jira.jboss.org/jira/browse/JBIDE-3211 
		 * Storing global preferences to instance scope to
		 * ${workspace}\.metadata\.plugins\org.eclipse.core.runtime\
		 * .settings\org.jboss.tools.common.model.prefs
		 */
		IScopeContext instanceContext = new InstanceScope();
		Preferences root = instanceContext.getNode(ModelPlugin.PLUGIN_ID);
		if (null != root) {
			Preferences node = root.node(ResourceReferencePlugin.PLUGIN_ID);
			node.put(getPropertyName().getLocalName(), sb.toString());
			try {
				root.flush();
			} catch (BackingStoreException e) {
				ResourceReferencePlugin.getPluginLog().logError(e);
			}
		}
	}

	public ResourceReference[] getAllResources(IPath path) {
		Set locations = new HashSet();
		List css = new ArrayList();
		IPath parent = path.removeLastSegments(1);
		int depth = 0;
		boolean isGlobal = path.equals(Platform.getLocation());
		int setScope = isGlobal ? ResourceReference.GLOBAL_SCOPE : ResourceReference.FILE_SCOPE;
		while(parent != null && parent.segmentCount() > 1) {
			String[] dcss = getDeclaredResources(path);
			for (int i = 0; i < dcss.length; i++) {
				if(locations.contains(dcss[i])) continue;
				ResourceReference ref = new ResourceReference(dcss[i],isGlobal ? ResourceReference.GLOBAL_SCOPE :  ResourceReference.FOLDER_SCOPE);
				if(isGlobal){
				    ref.setGlobal(true);
				}
				ref.setDepth(depth);
				locations.add(dcss[i]);
				css.add(ref);
			}
			parent = parent.removeLastSegments(1);
			depth++;
		}
		String[] dcss = getDeclaredResources(path);
		for (int i = 0; i < dcss.length; i++) {
			if(locations.contains(dcss[i])) continue;
			ResourceReference ref = new ResourceReference(dcss[i], setScope);
			if(setScope == ResourceReference.GLOBAL_SCOPE){
			    ref.setGlobal(true);
			}
			locations.add(dcss[i]);
			css.add(ref);
		}
		return (ResourceReference[])css.toArray(new ResourceReference[0]);
	}

	private String[] getDeclaredResources(IPath path) {
	    String s = (String)getAllExternalResources().get(path.toString());
		if(s == null || s.length() == 0) {
			return new String[0];
		}else {
			return	decodeResourceString(s);
		}
	}

	public void setAllResources(IPath path, ResourceReference[] entries) {
		IPath changed = null;
		boolean b = false;
		int checkScope = path.equals(Platform.getLocation()) ? ResourceReference.GLOBAL_SCOPE : ResourceReference.FILE_SCOPE;

		b = setDeclaredResources(path, entries, checkScope, 0);
		if(b) {
			changed = path;
		}
		IPath parent = path.removeLastSegments(1);
		int depth = 0;
		while(parent != null && parent.segmentCount() > 1) {
			b = setDeclaredResources(parent, entries, ResourceReference.FOLDER_SCOPE, depth);
			if(b) {
				changed = parent;
			}
			parent = parent.removeLastSegments(1);
			depth++;
		}
		if(changed != null) {
			setAllExternalResources();
			fire(changed);
		}
	}

	private boolean setDeclaredResources(IPath path, ResourceReference[] entries, int scope, int depth) {
	    		String oldValue = (String)getAllExternalResources().get(path.toString());
			if(oldValue == null) {
			    oldValue = ""; //$NON-NLS-1$
			}
			String newValue = encodeDeclaredResources(entries, scope, depth);
			if(oldValue.equals(newValue)) return false;
			if(newValue == null || newValue.length() == 0) {
				getAllExternalResources().remove(path.toString());
			} else {
				getAllExternalResources().put(path.toString(), newValue);
			}
		return true;
	}

}
