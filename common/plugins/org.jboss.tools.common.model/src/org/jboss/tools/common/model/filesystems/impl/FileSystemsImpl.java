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

import java.io.*;
import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.internal.ui.*;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IContributorResourceAdapter;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.FilePathHelper;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;

public class FileSystemsImpl extends OrderedObjectImpl implements IResourceChangeListener {
    private static final long serialVersionUID = 1527918897367197051L;
    boolean needsUpdateOverlapping = false;
    protected Set<String> overlapped = null;
    Ov overlapper = null;
    FileSystemsRenameListener fileSystemsRenameListener = new FileSystemsRenameListener(this);

    public FileSystemsImpl() {
		IWorkspace workspace = ModelPlugin.getWorkspace();
		if (workspace != null) workspace.addResourceChangeListener(this);
    }

    public String getPresentationString() {
    	IProject p = EclipseResourceUtil.getProject(this);
		String app = getAttributeValue("application name"); //$NON-NLS-1$
    	if(p != null && !app.equals(p.getName())) {
    		app = app.length() > 0 ? p.getName() + " (" + app + ")" : p.getName(); //$NON-NLS-1$ //$NON-NLS-2$
    	}
        return (app != null && app.length() > 0) ? app : super.getPresentationString();
    }

    public boolean addChild_0(XModelObject o) {
        boolean b = super.addChild_0(o);
        if(b && o instanceof FileSystemImpl && o.isActive()) {
			((FileSystemImpl)o).getResource();
        }
        if(b) updateOverlappedLater();
        return b;
    }

    public void removeChild_0(XModelObject o) {
        super.removeChild_0(o);
        updateOverlappedLater();
    }
    
    private static int OV_SLEEPING = 0;
    private static int OV_STOPPED = 1;
    private static int OV_RUNNING = 2;

    class Ov implements XJob.XRunnable {
    	int status = OV_SLEEPING;
        public void run() {
        
          try {
        	  Thread.sleep(200);
          } catch (InterruptedException e) {
        	  return;
          }
          if(status == OV_STOPPED) return;
          status = OV_RUNNING;
          try {
        	  updateOverlappedInternal();
          } finally {
        	  overlapper = null;
          }
        }

		public String getId() {
			return "Model:update overlapped: " + XModelConstants.getWorkspace(FileSystemsImpl.this.getModel()); //$NON-NLS-1$
		}
    }

	private void updateOverlappedLater() {
		if(isActive() && overlapper == null) {
			if(EclipseResourceUtil.isProjectFragment(getModel())) return;
			needsUpdateOverlapping = true;
			XJob.addRunnable(overlapper = new Ov());
//			(overlapper = new Thread(new Ov())).start();
		}
	}
	
    public boolean updateOverlapped() {
    	if(overlapper != null && overlapper.status == OV_RUNNING) {
    		return false;
    	} else if(overlapper != null) {
    		overlapper.status = OV_STOPPED;
    		overlapper = null;
    	}
		updateOverlappedInternal();
		return true;
    }

	private //synchronized 
		void updateOverlappedInternal() {
		if(!needsUpdateOverlapping) return;
		needsUpdateOverlapping = false;
		if(EclipseResourceUtil.isProjectFragment(getModel())) return;
        if(overlapped == null) overlapped = new HashSet<String>();
        Set<String> _overlapped = new HashSet<String>();
        Map<String,String> overlappedSystems = new HashMap<String,String>();
        XModelObject[] cs = getChildren();
        String[] paths = new String[cs.length];
        for (int i = 0; i < cs.length; i++) {
            String path = XModelObjectUtil.getExpandedValue(cs[i], XModelObjectConstants.ATTR_NAME_LOCATION, null);
            try {
                File f = new File(path);
                path = f.getCanonicalPath().replace('\\', '/');
                path = FilePathHelper.toPathPath(path);
                if (path.charAt(path.length()-1) != '/') path += '/';
                paths[i] = path;
            } catch (IOException e) {
                paths[i] = null;
            }
        }
        for (int i = 0; i < paths.length; i++) {
            if(paths[i] == null) continue;
            for (int j = 0; j < paths.length; j++) {
                if(i == j || paths[j] == null) continue;
                if(!paths[i].startsWith(paths[j])) continue;
                if(paths[i].equals(paths[j]) && j < i) continue;
                String overlap = cs[j].getPathPart() + paths[i].substring(paths[j].length()-1, paths[i].length()-1);
                _overlapped.add(overlap);
                overlappedSystems.put(overlap, cs[i].getPath());
            }
        }
        List<XModelObject> fire = new ArrayList<XModelObject>();
        Iterator it = overlapped.iterator();
        while(it.hasNext()) {
            String path = (String)it.next();
            if(_overlapped.contains(path)) {
				XModelObject c = getChildByPath(path);
				if(c == null || XModelObjectConstants.TRUE.equals(c.get("overlapped"))) { //$NON-NLS-1$
					_overlapped.remove(path);
				}
            } else {
                it.remove();
                XModelObject c = getChildByPath(path);
                if(c == null) continue;
                c.set("overlapped", ""); //$NON-NLS-1$ //$NON-NLS-2$
                c.set("overlappedSystem", ""); //$NON-NLS-1$ //$NON-NLS-2$
                c = c.getParent();
                if(c != null) fire.add(c);
            }
        }
        it = _overlapped.iterator();
        while(it.hasNext()) {
            String path = (String)it.next();
            overlapped.add(path);
            XModelObject c = getChildByPath(path);
            if(c == null) continue;
            c.set("overlapped", XModelObjectConstants.TRUE); //$NON-NLS-1$
            c.set("overlappedSystem", "" + overlappedSystems.get(path)); //$NON-NLS-1$ //$NON-NLS-2$
            c = c.getParent();
            if(c != null) fire.add(c);
        }
        XModelObject[] os = (XModelObject[])fire.toArray(new XModelObject[0]);
        for (int i = 0; i < os.length; i++)
          ((XModelImpl)getModel()).fireStructureChanged(os[i]);
        if(overlapped.size() == 0) overlapped = null;
    }

    public boolean isOverlapped(XModelObject o) {
        return (overlapped != null && overlapped.contains(o.getPath()));
    }

    public String get(String name) {
        if("APPLICATION_NAME".equals(name) || "application-name".equals(name)) { //$NON-NLS-1$ //$NON-NLS-2$
            String s = super.get(name);
            if(s == null || s.length() == 0) {
            	s = ""; //$NON-NLS-1$
            	// project name ?
            }
            return (s == null) ? "" : s; //$NON-NLS-1$
        } else {
            return super.get(name);
        }
    }

    public void forceUpdate() {
    	if(currentUpdate != null) {
    		currentUpdate.run();
    	}
    }

	public void resourceChanged(IResourceChangeEvent event) {
		if(!isActive() || event == null || event.getDelta() == null) return;
		if(!checkDelta(event.getDelta())) return;
		fileSystemsRenameListener.checkFileSystemRename(event);

		requireUpdate();
	}
	
	UpdateRunnable currentUpdate = null;
	
	void requireUpdate() {
		if(lock == 0) {
//			synchronized (this) {
				currentUpdate = new UpdateRunnable();
//			}			
			XJob.addRunnableWithPriority(currentUpdate);
		} else {
//			synchronized (this) {
				if(currentUpdate != null) currentUpdate.request++;
//			}
		}
	}
	
	boolean checkDelta(IResourceDelta delta) {
		IResource resource = delta.getResource();
		if(resource == null) return false;
		if(resource instanceof IWorkspaceRoot) {
			IResourceDelta[] d = delta.getAffectedChildren();
			return (d.length > 0 && checkDelta(d[0]));
		} else {
			IProject p = resource.getProject();
			IProject cp = EclipseResourceUtil.getProject(this);
			if(cp != null && cp != p && p != null) {
				return false;
			} 
		}
		return true;
	}
	
	int lock = 0;
	
	public void lockUpdate() {
		lock++;
	}
	
	public void unlockUpdate() {
		lock--;
	}
	
	private boolean isUpdating = false;
	boolean saveRequested = false;
	
	public boolean requestSave() {
		if(isUpdating) saveRequested = true;
		return isUpdating;
	}

	class UpdateRunnable implements XJob.XRunnable {
		String id = "Update File Systems - " + XModelConstants.getWorkspace(getModel()); //$NON-NLS-1$
		int request = 0;
		int usage = 0;

		public String getId() {
			return id;
		}

		public void run() {
			if(this != currentUpdate) return;
			if(!isActive()) {
				ModelPlugin.getWorkspace().removeResourceChangeListener(FileSystemsImpl.this);
			} else {
				doUpdate();
			}
			if(this == currentUpdate) currentUpdate = null;
		}
		
	}
	
	private void doUpdate() {
		if(lock > 0) return;
		isUpdating = true;
		try {
			boolean b = isOpenProject();
			if(b) {
				XModelObjectLoaderUtil.getObjectLoader(FileSystemsImpl.this).update(FileSystemsImpl.this);
			} 
		} catch (XModelException e) {
			ModelPlugin.getPluginLog().logError(e);
		}
		if(saveRequested) {
			try {
				XModelObjectLoaderUtil.getObjectLoader(FileSystemsImpl.this).save(FileSystemsImpl.this);
			} finally {
				saveRequested = false;
				isUpdating = false;
			}
		}
		isUpdating = false;
	}
	
	private boolean isOpenProject() {
		IProject p = EclipseResourceUtil.getProject(this);
		return p != null && p.isAccessible() && p.isOpen();		
	}
	
	private IContributorResourceAdapter contributorResourceAdapter = null;

	public Object getAdapter(Class adapter) {
		if(IResource.class == adapter || IProject.class == adapter) return EclipseResourceUtil.getProject(this);
		if(IContributorResourceAdapter.class == adapter) {
			if(contributorResourceAdapter == null) {
				contributorResourceAdapter = new ExtendedJavaElementAdapterFactory();
			}
			return contributorResourceAdapter;
		} else if(IWorkbenchAdapter.class == adapter) {
			return new ExtendedJavaWorkbenchAdapter();
		} else if(IContributorResourceAdapter.class == adapter) {
			return new IContributorResourceAdapterImpl();
		}
		return super.getAdapter(adapter);
	}
	

	private IAdaptable validateAdaptable(IAdaptable adaptable) {
		IAdaptable a = null;
		if(adaptable instanceof XModelObject) {
			IProject p =  EclipseResourceUtil.getProject(FileSystemsImpl.this);
			if(p != null) a = EclipseResourceUtil.getJavaProject(p);
		}
		return a == null ? adaptable : a;
	}

	class ExtendedJavaElementAdapterFactory extends JavaElementAdapterFactory {
		
	    public IResource getAdaptedResource(IAdaptable adaptable) {
	    	return super.getAdaptedResource(validateAdaptable(adaptable));
	    }
	    
//	    public ResourceMapping getAdaptedResourceMapping(IAdaptable adaptable) {
//	    	return super.getAdaptedResourceMapping(validateAdaptable(adaptable));
//	    }

	}
	
	class ExtendedJavaWorkbenchAdapter extends JavaWorkbenchAdapter {
		public Object[] getChildren(Object element) {
			return super.getChildren(getJavaElement(element));
		}
		public ImageDescriptor getImageDescriptor(Object element) {
			return super.getImageDescriptor(getJavaElement(element));
		}

		public String getLabel(Object element) {
			return super.getLabel(getJavaElement(element));
		}

		public Object getParent(Object element) {
			return super.getLabel(getJavaElement(element));
		}
		private IJavaElement getJavaElement(Object element) {
			if (element instanceof IJavaElement)
				return (IJavaElement)element;
			IProject p =  EclipseResourceUtil.getProject(FileSystemsImpl.this);
			return EclipseResourceUtil.getJavaProject(p);
		}
	}
	
	class IContributorResourceAdapterImpl implements IContributorResourceAdapter {
		public IResource getAdaptedResource(IAdaptable adaptable) {
			return EclipseResourceUtil.getProject(FileSystemsImpl.this);
		}
	}

}

