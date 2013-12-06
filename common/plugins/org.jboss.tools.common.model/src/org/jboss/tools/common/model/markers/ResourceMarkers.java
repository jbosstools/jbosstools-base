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

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.widgets.Display;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FolderImpl;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.model.util.PositionHolder;

public class ResourceMarkers {
	public static String TEXT_PROBLEM = "org.jboss.tools.common.model.textproblemmarker"; //$NON-NLS-1$
	public static String CONSTRAINT_PROBLEM = "org.jboss.tools.jst.web.constraintsmarker"; //$NON-NLS-1$
	public static String JST_WEB_PROBLEM = "org.jboss.tools.jst.web.strutsmarker"; //$NON-NLS-1$
	public static String OLD_CONSTRAINT_PROBLEM = "org.jboss.tools.jst.web.ui.constraintsmarker"; //$NON-NLS-1$
	public static String OLD_JST_WEB_PROBLEM = "org.jboss.tools.jst.web.ui.strutsmarker"; //$NON-NLS-1$
	private XModelObject object;
	
	String type = null;
	String oldType = null;
	
	public ResourceMarkers(String type) {
		this.type = type;
	}
	
	public ResourceMarkers(String type, String oldType) {
		this.type = type;
		this.oldType = oldType;
	}
	
	public void setModelObject(XModelObject object) {
		this.object = object;
	}
	
	public void update() {
		if(object == null || !object.isActive()) return;		
		update0();
	}
	
	private void update0() {
		Set<XMarker> dms = null;
		Set<XMarker> added = null;
		IResource r = EclipseResourceUtil.getResource(object);
		if(r == null || !r.exists()) return;

		if(object.getParent() instanceof FolderImpl) {
			if( ((FolderImpl)object.getParent()).isOverlapped() ) {
				return;
			}
		}

		dms = getOwnedMarkers(r);
		String[] errorList = getErrors();
		for (int i = 0; i < errorList.length; i++) {
			String error = errorList[i];
			if(error == null || error.length() == 0) continue;
			String message = getTrueMessage(error);
			String path = getObjectPathForError(i);
			int location = getLocation(i);
			if(location < 0) location = getLocation(error);
			String attr = getObjectAttributeForError(i);
			XMarker marker = findMarker(path, message, attr, dms);
			if(marker != null) {
				dms.remove(marker);
				continue;
			}
			marker = new XMarker();
			if(added == null) {
				added = new HashSet<XMarker>();
			}
			added.add(marker);
			marker.setType(type);
			marker.setMessage(message);
			marker.setPath(path);
			if(attr != null && attr.length() > 0) {
				marker.setAttribute(attr);
			}
		}
		if(r instanceof IFile) {
			IFile file = (IFile)r;
			if(dms != null) {
				XMarkerManager.getInstance().clearXMarkers(file, dms);
			}
			if(added != null) {
				XMarkerManager.getInstance().addXMarkers(file, added);
			}
			if((dms != null && !dms.isEmpty()) || (added != null && !added.isEmpty())) {
				XMarkerManager.getInstance().forceReload(file);
			}
		}
	}

	public static void updateLocation(IMarker marker, int location, int start, int end) throws CoreException {
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
	
	private XMarker findMarker(String path, String message, String attr, Set<XMarker> dms) {
		if(dms == null) return null;
		for (XMarker m: dms) {
			if(!message.equals(m.getMessage())) continue;
			if(attr != null && !attr.equals(m.getAttribute())) continue;
			if(oldType != null && oldType.equals(m.getType())) continue;
			if(!path.equals(m.getPath())) {
				m.setPath(path);
			}
			return m;
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
		Set<XMarker> ms = getOwnedMarkers((IFile)r);
		if(ms != null) {
			synchronized (XMarkerManager.getInstance()) {
				Set<XMarker> ms1 = XMarkerManager.getInstance().getMarkers((IFile)r);
				if(ms1 != null) {
					for (XMarker m: ms) ms1.remove(m);
				}
			}
		}		
	}
	
    private Set<XMarker> getOwnedMarkers(IResource r) {
    	if(!(r instanceof IFile)) {
    		return null;
    	}
    	Set<XMarker> l = null;
        synchronized (XMarkerManager.getInstance()) {
        	Set<XMarker> ms = XMarkerManager.getInstance().getMarkers((IFile)r);
        	if(ms != null) for (XMarker m: ms) {
        		if(isOwnedMarker(m)) {
        			if(l == null) l = new HashSet<XMarker>();
        			l.add(m);
        		}
        	}
        }
        return l;
    }

    boolean isOwnedMarker(XMarker m) {
    	if(m == null) return false;
    	String _type = m.getType();
    	if(_type == null) return true;
    	if(_type.startsWith("org.jboss.tools.")) { //$NON-NLS-1$
    		return _type.equals(type) || (oldType != null && _type.equals(oldType));
    	}
    	return false;
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
			if(ms != null) for (int i = 0; i < ms.length; i++) {
				String path = ms[i].getAttribute("path", null); //$NON-NLS-1$
				if(path == null) continue;
				XModelObject o = file.getModel().getByPath(path);
				if(o == null) {
					ms[i].delete();
					continue;
				}
				String attr = ms[i].getAttribute("attribute", null); //$NON-NLS-1$
				PositionHolder h = PositionHolder.getPosition(o, attr);
				h.update();
				updateLocation(ms[i], h.getLine(), h.getStart(), h.getEnd());
			}
		} catch (CoreException e) {
			//ignore
		}
	}

}
