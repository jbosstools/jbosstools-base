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
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.progress.UIJob;

import org.jboss.tools.common.model.XJob;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XJob.XRunnable;
import org.jboss.tools.common.model.filesystems.FileSystemsHelper;
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
	
	private Map<IFile, Set<XModelObject>> errorObjects = new HashMap<IFile, Set<XModelObject>>();
	private Map<IFile, Set<XModelObject>> warningObjects = new HashMap<IFile, Set<XModelObject>>();
	private Set<IFile> uptodate = new HashSet<IFile>();
	
	private XMarkerManager() {
		ModelPlugin.getWorkspace().addResourceChangeListener(this);
	}

	public void resourceChanged(IResourceChangeEvent event) {
		if((event.getType() == IResourceChangeEvent.PRE_DELETE
			|| event.getType() == IResourceChangeEvent.PRE_CLOSE)
				&& event.getResource() instanceof IProject) {
			IProject p = (IProject)event.getResource();
			clear(p.getFullPath());
			return;
		}
		IResourceDelta delta = event.getDelta();
		try {
			if(delta != null) {
				delta.accept(visitor);
			}
		} catch (CoreException e) {
			ModelPlugin.getDefault().logError(e);
		}
	}

	private synchronized void clear(IFile f) {
		uptodate.remove(f);
		errorObjects.remove(f);
		warningObjects.remove(f);
	}

	private synchronized void clear(IPath f) {
		Iterator<IFile> it = uptodate.iterator();
		while(it.hasNext()) {
			if(f.isPrefixOf(it.next().getFullPath())) {
				it.remove();
			}
		}
		it = errorObjects.keySet().iterator();
		while(it.hasNext()) {
			if(f.isPrefixOf(it.next().getFullPath())) {
				it.remove();
			}
		}
		it = warningObjects.keySet().iterator();
		while(it.hasNext()) {
			if(f.isPrefixOf(it.next().getFullPath())) {
				it.remove();
			}
		}
	}
	

	ResourceDeltaVisitor visitor = new ResourceDeltaVisitor();

	class ResourceDeltaVisitor implements IResourceDeltaVisitor {
		@Override
		public boolean visit(IResourceDelta delta) throws CoreException {
			IResource r = delta.getResource();
			if(delta.getKind() == IResourceDelta.REMOVED) {
				if(r instanceof IFile) {
					clear((IFile)r);
				} else {
					clear(r.getFullPath());
				}
				return false;
			} else if(delta.getKind() == IResourceDelta.ADDED) {
				return false;
			} else if(delta.getKind() == IResourceDelta.NO_CHANGE) {
				return true;
			} else {
				if(r instanceof IFile) {
					IFile f = (IFile)r;
					if(uptodate.contains(f)) {
						synchronized(this) {
							uptodate.remove(f);
						}
						if(f.exists()) {
							updateJob.add(f);
						}
					}
				}
			}
			return true;
		}		
	}


	UpdateJob updateJob = new UpdateJob();

	class UpdateJob extends UIJob {// implements XRunnable {		
		Set<IFile> fs = new HashSet<IFile>();
		boolean running = false;

		UpdateJob() {
			super(Display.getDefault(), "XMarkerManager");
		}

		public String getId() {
			return "XMarkerManager"; //$NON-NLS-1$
		}
//		public void run() {
		public IStatus runInUIThread(IProgressMonitor monitor) {
			synchronized (this) {
				running = true;
			}
//			try {
//				Thread.sleep(100);
//			} catch (InterruptedException e) {
//				return Status.OK_STATUS;
//			}
			try {
				IFile f = null;
				while((f = nextFile()) != null) {
					System.out.println("Reloading " + f);
					reload(f);
				}
			} finally {
				synchronized (this) {
					running = false;
				}
			}
			return Status.OK_STATUS;
		}

		synchronized IFile nextFile() {
			if(fs.isEmpty()) {
				return null;
			}
			IFile f = fs.iterator().next();
			fs.remove(f);
			return f;
		}
	
		public synchronized void add(IFile f) {
			if(fs.contains(f)) {
				return;
			}
			fs.add(f);
			if(!running) {
//				XJob.addRunnable(updateJob);
				running = true;
				schedule(100);
			}
		}		
	}
	
	public void reload(IFile file) {
		synchronized (this) {
			if(uptodate.contains(file)) {
				return;
			} else {
				uptodate.add(file);
			}
		}
		IMarker[] ms = new IMarker[0];
		try {
			ms = (file == null || !file.isAccessible()) 
				? ModelPlugin.getWorkspace().getRoot().findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE)
				: file.findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
		} catch (CoreException e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		Set<XModelObject> os;

		synchronized (this) { 
			os = errorObjects.get(file);
			if(os == null) {
				os = new HashSet<XModelObject>();
				errorObjects.put(file, os);
			}
		}
		reload(ms, os, IMarker.SEVERITY_ERROR);
		synchronized (this) { 
			os = warningObjects.get(file);
			if(os == null) {
				os = new HashSet<XModelObject>();
				warningObjects.put(file, os);
			}
		}
		reload(ms, os, IMarker.SEVERITY_WARNING);
	}
	
	void reload(IMarker[] ms, Set<XModelObject> objects, int severity) {
		Set<XModelObject> es = new HashSet<XModelObject>();
		for (int i = 0; i < ms.length; i++) {
			if(severity != ms[i].getAttribute(IMarker.SEVERITY, 0)) continue;
			IResource r = ms[i].getResource();
			XModelObject o = EclipseResourceUtil.getObjectByResource(r);
			if(o == null) o = EclipseResourceUtil.createObjectForResource(r);
			if(o == null) continue;
			String path = ms[i].getAttribute("path", null); //$NON-NLS-1$
			o = (path == null) ? o : o.getModel().getByPath(path);
			if(o == null) continue;
			es.add(o);
			String attr = ms[i].getAttribute("attribute", null); //$NON-NLS-1$
			if(attr != null && attr.length() > 0) {
				((XModelObjectImpl)o).addErrorAttributeDirty(attr);
			}
		}
		Set<XModelObject> copy = new HashSet<XModelObject>();
		Set<XModelObject> toRemove = new HashSet<XModelObject>();
		Set<XModelObject> toAdd = new HashSet<XModelObject>();
		
		synchronized(this) {
			copy.addAll(objects);
		}
		
		Iterator<XModelObject> it = copy.iterator();
		while(it.hasNext()) {
			XModelObject o = it.next();
			if(!es.contains(o)) {
				if(o.getErrorState() == severity) {
					o.setErrorState(0);
				}
				toRemove.add(o);
			} else if(es.contains(o)) {
				if(severity > o.getErrorState()) {
					o.setErrorState(severity);
				} else {
					((XModelObjectImpl)o).commitErrorAttributes();
				}
				es.remove(o);
			}
		}
		it = es.iterator();
		while(it.hasNext()) {
			XModelObject o = (XModelObject)it.next();
			if(severity > o.getErrorState()) {
				o.setErrorState(severity);
			} else {
				((XModelObjectImpl)o).commitErrorAttributes();
			}					
			if(!objects.contains(o)) {
				toAdd.add(o);
			}
		}

		synchronized(this) {
			objects.removeAll(toRemove);
			objects.addAll(toAdd);
		}
		
	}

	void update(XModelObject object) {
		XModelObject fo = FileSystemsHelper.getFile(object);
		if(fo != null) {
			IFile f = (IFile)fo.getAdapter(IFile.class);
			if(f != null && f.exists()) {
				reload(f);
			}
		}
	}
	
	public int getErrorState(XModelObject object) {
		if(object == null) return 0;
		update(object);
		if(object.getErrorState() == IMarker.SEVERITY_ERROR || object.getErrorChildCount() > 0) return IMarker.SEVERITY_ERROR;
		if(object.getErrorState() == IMarker.SEVERITY_WARNING || object.getWarningChildCount() > 0) return IMarker.SEVERITY_WARNING;
		return 0;
	}
	
	public boolean hasErrors(XModelObject object) {
		update(object);
		return object != null && (object.getErrorState() == IMarker.SEVERITY_ERROR || object.getErrorChildCount() > 0);
	}
	
	public boolean hasWarnings(XModelObject object) {
		update(object);
		return object != null && (object.getErrorState() == IMarker.SEVERITY_WARNING || object.getWarningChildCount() > 0);
	}
	
	public boolean hasErrors(XModelObject object, String attribute) {
		update(object);
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
		} catch (CoreException e) {
			//ignore no markers - no problem			
			return null;
		}
		if(ms == null) return null;
		String pathInFile = object.getPath().substring(f.getPath().length());
		for (int i = 0; i < ms.length; i++) {
			XModelObject o = object;
			String path = ms[i].getAttribute("path", null); //$NON-NLS-1$
			if(path != null && !path.endsWith(pathInFile)) {
				continue;
			}
			o = (path == null) ? o : o.getModel().getByPath(path);
			if(o == null) continue;
			String attr = ms[i].getAttribute("attribute", null); //$NON-NLS-1$
			if(attr != null && attr.equals(attribute)) {
				return ms[i].getAttribute(IMarker.MESSAGE, null);
			}
		}
		return null;
	}

}
