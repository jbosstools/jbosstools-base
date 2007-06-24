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
package org.jboss.tools.common.model.markers;

import java.util.*;
import org.eclipse.core.resources.*;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.impl.XModelObjectImpl;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

public class XMarkerManager implements IResourceChangeListener {
	
	private static XMarkerManager instance;
	
	public static XMarkerManager getInstance() {
		if(instance == null) {
			instance = new XMarkerManager();
		}
		return instance;
	}
	
	private Set<XModelObject> errorObjects = new HashSet<XModelObject>();
	private Set<XModelObject> warningObjects = new HashSet<XModelObject>();
	
	private XMarkerManager() {
		reload();
		ModelPlugin.getWorkspace().addResourceChangeListener(this);
	}

	public void resourceChanged(IResourceChangeEvent event) {
		reload();
	}
	
	public void reload() {
		IMarker[] ms = new IMarker[0];
		try {
			ms = ModelPlugin.getWorkspace().getRoot().findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
		} catch (Exception e) {
			ModelPlugin.log(e);
		}
		reload(ms, errorObjects, IMarker.SEVERITY_ERROR);
		reload(ms, warningObjects, IMarker.SEVERITY_WARNING);
	}
	
	void reload(IMarker[] ms, Set<XModelObject> objects, int severity) {
		Set<XModelObject> es = new HashSet<XModelObject>();
		for (int i = 0; i < ms.length; i++) {
			if(severity != ms[i].getAttribute(IMarker.SEVERITY, 0)) continue;
			IResource r = ms[i].getResource();
			XModelObject o = EclipseResourceUtil.getObjectByResource(r);
			if(o == null) o = EclipseResourceUtil.createObjectForResource(r);
			if(o == null) continue;
			String path = ms[i].getAttribute("path", null);
			o = (path == null) ? o : o.getModel().getByPath(path);
			if(o == null) continue;
			es.add(o);
			String attr = ms[i].getAttribute("attribute", null);
			if(attr != null && attr.length() > 0) ((XModelObjectImpl)o).addErrorAttributeDirty(attr);
		}
		synchronized(objects) {
			Iterator it = objects.iterator();
			while(it.hasNext()) {
				XModelObject o = (XModelObject)it.next();
				if(!es.contains(o)) {
					if(o.getErrorState() == severity) {
						o.setErrorState(0);
					}
					it.remove();
				} else if(es.contains(o)) {
					if(severity > o.getErrorState()) {
						o.setErrorState(severity);
					}
					es.remove(o);
				}
			}
			it = es.iterator();
			while(it.hasNext()) {
				XModelObject o = (XModelObject)it.next();
				if(severity > o.getErrorState()) {
					o.setErrorState(severity);
				}
				if(!objects.contains(o)) {
					objects.add(o);
				}
			}
		}
	}	
	
	public int getErrorState(XModelObject object) {
		if(object == null) return 0;
		if(object.getErrorState() == IMarker.SEVERITY_ERROR || object.getErrorChildCount() > 0) return IMarker.SEVERITY_ERROR;
		if(object.getErrorState() == IMarker.SEVERITY_WARNING || object.getWarningChildCount() > 0) return IMarker.SEVERITY_WARNING;
		return 0;
	}
	
	public boolean hasErrors(XModelObject object) {
		return object != null && (object.getErrorState() == IMarker.SEVERITY_ERROR || object.getErrorChildCount() > 0);
	}
	
	public boolean hasWarnings(XModelObject object) {
		return object != null && (object.getErrorState() == IMarker.SEVERITY_WARNING || object.getWarningChildCount() > 0);
	}
	
	public boolean hasErrors(XModelObject object, String attribute) {
		if(attribute == null) return hasErrors(object);
		if(object.getErrorState() == 0) return false;
		return object.getAttributeErrorState(attribute);
	}
	
	public String getError(XModelObject object, String attribute) {
		XModelObject f = ((XModelObjectImpl)object).getResourceAncestor();
		IFile file = (f == null) ? null : (IFile)f.getAdapter(IFile.class);
		if(file == null) return null;
		IMarker[] ms = null;
		try {
			ms = file.findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
		} catch (Exception e) {
			//ignore no markers - no problem			
			return null;
		}
		if(ms == null) return null;
		for (int i = 0; i < ms.length; i++) {
			XModelObject o = object;
			String path = ms[i].getAttribute("path", null);
			o = (path == null) ? o : o.getModel().getByPath(path);
			if(o == null) continue;
			String attr = ms[i].getAttribute("attribute", null);
			if(attr != null && attr.equals(attribute)) {
				return ms[i].getAttribute(IMarker.MESSAGE, null);
			}
		}
		return null;
	}

}
