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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.widgets.Display;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.PositionHolder;

public class ResourceMarkers {
	public static String TEXT_PROBLEM = "org.jboss.tools.common.model.textproblemmarker";
	public static String CONSTRAINT_PROBLEM = "org.jboss.tools.common.model.web.ui.constraintsmarker";
	public static String JST_WEB_PROBLEM = "org.jboss.tools.jst.web.ui.strutsmarker";
	private XModelObject object;
	
	String type;
	
	public ResourceMarkers(String type) {
		this.type = type;
	}
	
	public void setModelObject(XModelObject object) {
		this.object = object;
	}
	
	public void update() {
		if(object == null || !object.isActive()) return;		

		IWorkspaceRunnable r= new IWorkspaceRunnable() {
			public void run(IProgressMonitor monitor) throws CoreException {
				update0();
			}
		};
		
		try {
			ModelPlugin.getWorkspace().run(r, null,IWorkspace.AVOID_UPDATE, null);
		} catch (CoreException e) {
			///ModelPlugin.log(e);
		}
	}
	
	private void update0() {
		Set<IMarker> dms = null;
		try {
			IResource r = EclipseResourceUtil.getResource(object);
			if(r == null || !r.exists()) return;
			IMarker[] ms = getOwnedMarkers(r);
			if(ms != null) {
				dms = new HashSet<IMarker>();
				for (int i = 0; i < ms.length; i++) dms.add(ms[i]);
			}
			String[] errorList = getErrors();
			for (int i = 0; i < errorList.length; i++) {
				String error = errorList[i];
				if(error == null || error.length() == 0) continue;
				String message = getTrueMessage(error);
				String path = getObjectPathForError(i);
				int location = getLocation(i);
				if(location < 0) location = getLocation(error);
				String attr = getObjectAttributeForError(i);
				IMarker marker = findMarker(path, message, attr, ms);
				if(marker != null) {
					dms.remove(marker);
					updateLocation(marker, location, getStart(i), getEnd(i));
					continue;
				}
				marker = r.createMarker(type);
				marker.setAttribute(IMarker.MESSAGE, message);
				marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_ERROR);
				marker.setAttribute("path", path);
				if(attr != null && attr.length() > 0) {
					marker.setAttribute("attribute", attr);
				}
				updateLocation(marker, location, getStart(i), getEnd(i));
			}
			if(dms == null) return;
			ms = dms.toArray(new IMarker[0]);
			for (int i = 0; i < ms.length; i++) ms[i].delete(); 
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError(e);
		}
	}
	
	public static void updateLocation(IMarker marker, int location, int start, int end) throws Exception {
		if(location >= 0 && marker.getAttribute(IMarker.LINE_NUMBER, -1) != location) {
			marker.setAttribute(IMarker.LINE_NUMBER, location);
		}
		if(start >= 0 && marker.getAttribute(IMarker.CHAR_START, -1) != start) {
			marker.setAttribute(IMarker.CHAR_START, start);
		}
		if(end >= 0 && marker.getAttribute(IMarker.CHAR_END, -1) != end) {
			marker.setAttribute(IMarker.CHAR_END, end);
		}
	}
	
	private IMarker findMarker(String path, String message, String attr, IMarker[] ms) {
		if(ms == null) return null;
		for (int i = 0; i < ms.length; i++) {
			try {
				if(!message.equals(ms[i].getAttribute(IMarker.MESSAGE))) continue;
				if(attr != null && !attr.equals(ms[i].getAttribute("attribute"))) continue;
				if(!path.equals(ms[i].getAttribute("path"))) {
					ms[i].setAttribute("path", path);
				}
				return ms[i];
			} catch (Exception e) {
				//ignore
				continue;
			}
		}
		return null;
	}
	
	protected String getObjectPathForError(int i) {
		return object == null ? null : object.getPath();
	}
	
	protected String getObjectAttributeForError(int i) {
		return null;
	}
	
	protected String getTrueMessage(String message) {
		StringBuffer sb = new StringBuffer();
		int c = 0;
		while(c < message.length()) {
			int i1 = message.indexOf('@', c);
			if(i1 < 0) break;
			int i2 = message.indexOf('@', i1 + 1);
			if(i2 < 0) break;
			int i3 = message.indexOf('@', i2 + 1);
			if(i3 < 0) break;
			if(c > 0) sb.append(message.substring(0, i1));
			sb.append(message.substring(i2 + 1, i3));
			c = i3 + 1;
		}
		sb.append(message.substring(c));
		return sb.toString();		
	}
	
	public void clear() {
		IResource r = EclipseResourceUtil.getResource(object);
		if(!(r instanceof IFile)) return;
		final IMarker[] ms = getOwnedMarkers(r);
		if(ms == null) return;
		IWorkspaceRunnable runnable = new IWorkspaceRunnable() {
			public void run(IProgressMonitor monitor) throws CoreException {
				try {
					for (int i = 0; i < ms.length; i++) ms[i].delete();
				} catch (Exception e) {
					//ignore
				}
			}
		};
		
		try {
			ModelPlugin.getWorkspace().run(runnable, null,IWorkspace.AVOID_UPDATE, null);
		} catch (CoreException e) {
			ModelPlugin.getPluginLog().logError(e);
		}
	}
	
	private IMarker[] getOwnedMarkers(IResource r) {
		ArrayList<IMarker> l = null;
		try {
			IMarker[] ms = r.findMarkers(null, false, 1);
			for (int i = 0; i < ms.length; i++) {
				if(isOwnedMarker(ms[i])) {
					if(l == null) l = new ArrayList<IMarker>();
					l.add(ms[i]);
				}
			}
		} catch (Exception e) {
			//ignore
		}
		return (l == null) ? null : l.toArray(new IMarker[0]);
	}
	
	protected boolean isOwnedMarker(IMarker m) throws Exception {
		String _type = m.getType();
		if(_type == null) return true;
		if(_type.startsWith("org.jboss.tools.")) {
			return _type.equals(type);
		}
		return m.isSubtypeOf(IMarker.TEXT) || m.isSubtypeOf(IMarker.PROBLEM);
	}
	
	protected String[] getErrors() {
		return new String[0];	
	}

	protected int getLocation(int i) {
		return -1;
	}
	
	protected int getLocation(String error) {
		return -1;
	}
	
	protected int getStart(int i) {
		return -1;
	}
	
	protected int getEnd(int i) {
		return -1;
	}
	

	public static void refreshProblemMarkersAsync(final XModelObject file) {
		if(!file.isActive()) return;		
		Display.getDefault().asyncExec(new Runnable(){
			public void run() {
				refreshProblemMarkers(file);
			}
		});		
	}

	public static void refreshProblemMarkers(XModelObject file) {
		IResource r = (IResource)file.getAdapter(IResource.class);
		if(r == null) return;
		try {
			IMarker[] ms = r.findMarkers(ResourceMarkers.JST_WEB_PROBLEM, true, 1);
			for (int i = 0; i < ms.length; i++) {
//				String type = ms[i].getType();
				String path = ms[i].getAttribute("path", null);
				if(path == null) continue;
				XModelObject o = file.getModel().getByPath(path);
				if(o == null) {
					ms[i].delete();
					continue;
				}
				String attr = ms[i].getAttribute("attribute", null);
				PositionHolder h = PositionHolder.getPosition(o, attr);
				h.update();
				ResourceMarkers.updateLocation(ms[i], h.getLine(), h.getStart(), h.getEnd());
			}
		} catch (Exception e) {
			//ignore
		}
	}	
	
}
