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

import java.io.File;
import java.io.IOException;
import java.text.MessageFormat;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.eclipse.core.internal.resources.ResourceException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.widgets.Display;
import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.meta.impl.XMetaDataConstants;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModelException;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.filesystems.BodySource;
import org.jboss.tools.common.model.filesystems.FileAuxiliary;
import org.jboss.tools.common.model.filesystems.FilePathHelper;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.impl.RegularObjectImpl;
import org.jboss.tools.common.model.impl.XModelImpl;
import org.jboss.tools.common.model.loaders.AuxiliaryLoader;
import org.jboss.tools.common.model.loaders.Reloadable;
import org.jboss.tools.common.model.loaders.XObjectLoader;
import org.jboss.tools.common.model.loaders.impl.PropertiesLoader;
import org.jboss.tools.common.model.markers.ResourceMarkers;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.Paths;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;
import org.jboss.tools.common.util.FileUtil;

public class FolderImpl extends RegularObjectImpl implements FolderLoader {
    private static final long serialVersionUID = 8082046187736790127L;
    protected boolean loaded = false;
	protected IContainer resource = null;
	protected String pathForResource = null;
	LinkedResources linked = new LinkedResources();
//	protected Map<String, File> linked = new HashMap<String, File>();
//	protected Map<String, IResource> linkedResources = new HashMap<String, IResource>();

    public FolderImpl() {}

    public int getFileType() {
        return FOLDER;
    }
    
    protected Comparator<XModelObject> createComparator() {
        return new FileObjectComparator();
    }

    protected FileSystemImpl getFileSystem() {
        FolderImpl folder = (FolderImpl)getParent();
        return (folder == null) ? null : folder.getFileSystem();
    }

    public boolean isChildEditable(XModelObject o) {
        if(!isObjectEditable()) return false;
        File f = getChildIOFile(o);
        if(f == null) return true;
        return (!f.isFile() || f.canWrite());
    }

    protected File getFile() {
        String path = getAbsolutePath();
        return (path == null) ? null :
               new File(Paths.expand(path, getModel().getProperties()));
    }

    public BodySource getBodySource(String filename) {
        File f = getFile();
        return (f == null) ? null : getBodySource(getChildIOFile(filename));
    }

	BodySource getBodySource(File f) {
		IFile ef = getChildFile(f.getName());
		if(ef != null) return new EclipseFileBodySource(ef, f); 
		return new FileBodySource(f);
	}
    
    public boolean isAttributeEditable(String name) {
        return false;
    }

    protected String getAbsolutePath() {
    	FolderImpl parent = (FolderImpl)getParent();
        String p = (parent == null) ? null : parent.getAbsolutePath();
        if(parent.linked.containsFile(getPathPart())) {
        	return parent.linked.getFileByFileName(getPathPart()).getAbsolutePath();
        }
        return (p == null) ? null : p + XModelObjectConstants.SEPARATOR + name();
    }
    
	public IProject getProject() {
		return (getParent() == null) ? null : ((FolderImpl)getParent()).getProject();
	}

    private File[] getFiles() {
        File f = getFile();
        if(f == null) return null;
        if (!f.isDirectory()) return new File[0];
        File[] fs = f.listFiles();
        return (fs == null) ? new File[0] : fs;
    }

    public void set(String name, String value) {
        if(XModelObjectConstants.XML_ATTR_NAME.equals(name) && isActive()) {
            if(value != null && !value.equals(get(name))) copy();
        }
        super.set(name, value);
    }

    protected void loadChildren() {
        if(loaded || !isActive()) return;
        File[] fs = getFiles();
        IResource[] rs = getResources();        
        if(fs == null || rs == null) return;
        loaded = true;
        ((XModelImpl)getModel()).addLoader();
        try {
        	loadChildren(fs, rs);
        } finally {
			((XModelImpl)getModel()).removeLoader();
        }
    }
	private void loadChildren(File[] fs, IResource[] rs) {
		FileSystemPeer peer = getFileSystem().getPeer();
        for (int i = 0; i < fs.length; i++) {
        	_loadChild(peer, fs[i]);
        }
        for (int i = 0; i < rs.length; i++) {
        	if(!rs[i].isAccessible()) continue;
        	if(!rs[i].isLinked()) continue;
        	if(rs[i].getLocation() == null) {
//        		System.out.println("no location at link " + rs[i]);
        		continue;
        	}
        	File f = rs[i].getLocation().toFile();
        	linked.registerResource(rs[i]);
        	_loadChild(peer, f);
        }
        
        bindAuxiliary();

        fire = true;
    }

	private void bindAuxiliary() {
        XModelObject[] cs = getChildren();
        for (int i = 0; i < cs.length; i++) {
        	if(cs[i].getFileType() == XModelObject.FILE) {
        		XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(cs[i]);
                if(loader instanceof AuxiliaryLoader) {
                	((AuxiliaryLoader)loader).bind(cs[i]);
                }
        	}
        }
	}
	
	private void _loadChild(FileSystemPeer peer, File f) {
        if(f.isDirectory()) {
            Properties p = new Properties();
            p.setProperty(XModelObjectConstants.ATTR_NAME, f.getName());
            XModelObject c = getModel().createModelObject("FileFolder", p); //$NON-NLS-1$
            String pp = FilePathHelper.toPathPath(f.getName());
            if(linked.containsFile(pp)) {
            	c.setObject("file", linked.getFileByFileName(pp)); //$NON-NLS-1$
            }
            addChild(c);
        } else {
            createFileObject(f);
        }
        peer.register(f);
	}

    private Properties getEntityProperties(File f) {
        Properties p = new Properties();
        parseFileName(p, f.getName());
        String ext = p.getProperty(XModelObjectConstants.ATTR_NAME_EXTENSION);
        String body = null;
        String entity = getModel().getEntityRecognizer().getEntityName(ext, body);
        if("FileAny".equals(entity)) { //$NON-NLS-1$
        	boolean isText = XModelObjectLoaderUtil.isTextFile(f, 100);
            if(f.length() > 100000 || !isText) entity = XModelObjectConstants.ENT_FILE_ANY_LONG;
            else if(isText) entity = "FileTXT"; //$NON-NLS-1$
        } else /*if(entity == null)*/ {
            body = getBodySource(f).get();
            entity = getModel().getEntityRecognizer().getEntityName(ext, body);
        }
        if(entity == null || getModel().getMetaData().getEntity(entity) == null) entity = "FileAny"; //$NON-NLS-1$
        p.setProperty(XMetaDataConstants.ENTITY, entity);
        if(body != null) p.setProperty(XModelObjectConstants.ATTR_NAME_BODY, body);
        return p;
    }

    private void createFileObject(File f) {
        createFileObject(f, getEntityProperties(f));
    }

    private void createFileObject(File f, Properties p) {
    	BodySource bs = getBodySource(f);
        String body = p.getProperty(XModelObjectConstants.ATTR_NAME_BODY);
        String entity = p.getProperty(XMetaDataConstants.ENTITY);
        XModelObject c = getModel().createModelObject(entity, p);
        if(c == null) {
        	ModelPlugin.getPluginLog().logInfo("Cannot create file for entity " + entity); //$NON-NLS-1$
        	return;
        }
        if(isLateloadFile2(c)) {
            ((FileAnyImpl)c).setBodySource(bs);
        } else {
            XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(c);
            if(loader != null) {
                if(body == null) body = bs.get();
                XModelObjectLoaderUtil.setTempBody(c, body);
                	if("FilePROPERTIES".equals(entity) && bs instanceof EclipseFileBodySource) { //$NON-NLS-1$
                		String encoding = FileUtil.getEncoding(((EclipseFileBodySource)bs).ef);
                		if(encoding != null) c.set("_encoding_", encoding); //$NON-NLS-1$
                	}
                loader.load(c);
            } else if(c.getModelEntity().getAttribute(XModelObjectConstants.ATTR_NAME__FILE) != null) {
                c.set(XModelObjectConstants.ATTR_NAME__FILE, f.getAbsolutePath());
            }
        }
        if(linked.filesByFileName.containsValue(f)) {
        	c.setObject("file", f); //$NON-NLS-1$
        }
        addChild(c);
    }

	int updateLock = 0;
	Set<String> unsynchronized = null;
    public boolean update() {
        if(!loaded) return true;
        if(updateLock > 0) return true;
        updateLock++;
        Map<String,File> mf = new HashMap<String,File>();
        linked.clearFiles();
        XModelObject fileSystem = getFileSystem();
        if(fileSystem == null) return false;
		FileSystemsImpl fsi = (FileSystemsImpl)fileSystem.getParent();
      try {  
        if(resource != null && resource.exists() && !resource.isSynchronized(IResource.DEPTH_ONE)) try	{
			fsi.lockUpdate();
			resource.refreshLocal(IResource.DEPTH_ZERO, null);
			if(resource.exists()) {
				IResource[] rs = resource.members();
				for (int i = 0; i < rs.length; i++) {
					if(!rs[i].isSynchronized(IResource.DEPTH_ZERO)) {
						if(unsynchronized == null) unsynchronized = new HashSet<String>();
						String pp = FilePathHelper.toPathPath(rs[i].getName());
						unsynchronized.add(pp);
					}
				}
				if(resource.exists()) {
					resource.refreshLocal(IResource.DEPTH_ONE, null);
				}
			}
        } catch (ResourceException re) {
			IPath p = resource.getLocation();
			if(p != null && p.toFile().exists()) {
				ModelPlugin.getPluginLog().logError("Exception caught in FolderImpl.update()", re); //$NON-NLS-1$
			} else {
				//ignore we cannot prevent this when project is removed externally
			}   	
		} catch (CoreException e) {
	    	  ModelPlugin.getPluginLog().logError("Exception caught in FolderImpl.update()", e); //$NON-NLS-1$
		} finally {
			fsi.unlockUpdate();
		}
		
		
		try {
			if(resource != null && resource.exists()) {
				IResource[] rs = resource.members();
				for (int i = 0; i < rs.length; i++) {
					if(rs[i].isLinked()) {
						File f = rs[i].getLocation().toFile();
						String p = FilePathHelper.toPathPath(f.getName());
						mf.put(p, f);
						linked.registerResource(rs[i]);
					}
				}			
			}
		} catch (CoreException ex) {
	    	  ModelPlugin.getPluginLog().logError("Exception caught in FolderImpl.update()"); //$NON-NLS-1$
		}
		
        File[] fs = getFiles();
        for (int i = 0; i < fs.length; i++) {
			String p = FilePathHelper.toPathPath(fs[i].getName());
        	mf.put(p, fs[i]);
        }

        Map<String,XModelObject> mc = children.getObjectsMap();

        updateAuxiliary(mc, mf);

        Map<String,XModelObject> toRemove = new HashMap<String,XModelObject>();
        Iterator<String> io = mc.keySet().iterator();
        while(io.hasNext()) {
            String nm = io.next();
            if(mf.containsKey(nm)) continue;
            XModelObject o = (XModelObject)mc.get(nm);
            File of = getChildIOFile(o);
            if(o.getFileType() == XModelObject.FOLDER) {
                if(!getFileSystem().getPeer().containsDir(of)) continue;
            } else {
                if(!getFileSystem().getPeer().contains(of)) continue;
            }
            toRemove.put(nm, o);
            io.remove();
        }
        
        Iterator<String> it = mf.keySet().iterator();
        while(it.hasNext()) {
            String nm = it.next();
            File f = (File)mf.get(nm);
            XModelObject o = (XModelObject)mc.get(nm);
            if(o != null) {
                updateLoaded(o, f);
                mc.remove(nm);
            } else {
                updateNew(nm, f, toRemove);
            }
        }
        it = toRemove.keySet().iterator();
        while(it.hasNext()) {
            String nm = (String)it.next();
            updateRemove((XModelObject)toRemove.get(nm));
        }
        
        bindAuxiliary();
      } catch (NoClassDefFoundError error) {
    	  //Most probably Eclipse is shutting down.
    	  return true;
      } catch (XModelException t) {
    	  ModelPlugin.getPluginLog().logError("Exception caught in FolderImpl.update()"); //$NON-NLS-1$
      } finally {  
		updateLock--;
		unsynchronized = null;
      }
        return true;
    }
    
    protected File getChildIOFile(XModelObject o) {
        String s = FileAnyImpl.toFileName(o);
        File f = (File)o.getObject("file"); //for links //$NON-NLS-1$
        if(f == null && linked.containsFile(o.getPathPart())) {
        	f = linked.getFileByFileName(o.getPathPart());
        }
        if(f == null) {
        	f = new File(getFile(), s);
        }
        return f;
    }

    protected File getChildIOFile(String filename) {
        File f = null;
		String p = FilePathHelper.toPathPath(filename);
        if(linked.containsFile(p)) {
        	f = linked.getFileByFileName(p);
        }
        if(f == null) {
        	f = new File(getFile(), filename);
        }
        return f;
    }

    private void updateAuxiliary(Map<String,XModelObject> mc, Map<String,File> mf) throws XModelException {
        Iterator<String> it = mf.keySet().iterator();
        while(it.hasNext()) {
            String nm = (String)it.next();
            File f = mf.get(nm);
            XModelObject o = mc.get(nm);
            if(o == null || !o.getModelEntity().getName().equals(FileAuxiliary.AUX_FILE_ENTITY)) continue;
            it.remove();
            FileAnyAuxiliaryImpl aux = (FileAnyAuxiliaryImpl)o;
            mc.remove(nm);
            if(aux.isObsolete()) {
                FileAuxiliary h =  aux.getAuxiliaryHelper();
                XModelObject main = aux.getMainObject();
                if(main != null && main.isActive() && h != null) {
                	String n = h.getAuxiliaryName(main);
                	String p = n + "." + aux.getAttributeValue(XModelObjectConstants.ATTR_NAME_EXTENSION); //$NON-NLS-1$
                	XModelObject other = getChildByPath(p);
                	if(other != null && other != aux) other.removeFromParent();                	
                	aux.fileRenamed(n, aux.getAttributeValue(XModelObjectConstants.ATTR_NAME_EXTENSION));
                	aux.updateBodySource();
                   	if(!isOverlapped()) {
                   		File r = new File(f.getParentFile(), FileAnyImpl.toFileName(aux));
                   		if(!r.equals(f) && f.exists()) {
                   			if(!r.exists()) {
                   				FileUtil.copyFile(f, r);
                   				f.delete();
                                FileSystemPeer peer = getFileSystem().getPeer();
                                peer.unregister(f);
                                peer.register(r);
                                
                   				IFile ef = getChildFile(f.getName());
                   				if(ef != null && !ef.isSynchronized(0)) {
                   					try {
                   						ef.refreshLocal(0, null);
                   					} catch (CoreException e) {
                   			    	  ModelPlugin.getPluginLog().logError("Exception caught in FolderImpl.update()"); //$NON-NLS-1$
                   					}
                   				}
                   				ef = getChildFile(r.getName());
                   				if(ef != null && !ef.isSynchronized(0)) {
                   					try {
                   						ef.refreshLocal(0, null);
                   					} catch (CoreException e) {
                   			    	  ModelPlugin.getPluginLog().logError("Exception caught in FolderImpl.update()"); //$NON-NLS-1$
                   					}
                   				}
                   				
                   			} else {
                   				f.delete();
                                getFileSystem().getPeer().unregister(f);
                   				IFile ef = getChildFile(f.getName());
                   				if(ef != null && !ef.isSynchronized(0)) {
                   					try {
                   						ef.refreshLocal(0, null);
                   					} catch (CoreException e) {
                   						//ignore
                   					}
                   				}
                   			}
                   		}
                   	}
                } else {
                   	if(!isOverlapped()) {
                   		updateRemove(aux);
                   		f.delete();
                        getFileSystem().getPeer().unregister(f);
                   	}
                }
            } else {
                FileSystemPeer peer = getFileSystem().getPeer();
            	if(!registerFileInPeer(peer, f)) continue;
                XModelObject main = aux.getMainObject();
                if(main == null) continue;
                File fmain = (File)mf.get(main.getPathPart());
                if(fmain == null) continue;
                peer.unregister(fmain);
                setForceLoadProperty(main, true);
                updateLoaded(main, fmain);
                setForceLoadProperty(main, false);
                mc.remove(main.getPathPart());
            }
        }
    }
    
    //returns true if registration was really done
    private synchronized boolean registerFileInPeer(FileSystemPeer peer, File f) {
        if(f.isFile() && peer.contains(f) && !peer.isUpdated(f)) return false;
        peer.register(f);
    	return true;
    }
    
    private void setForceLoadProperty(XModelObject f, boolean b) {
    	f.set("forceLoad", b ? XModelObjectConstants.TRUE : "");    	 //$NON-NLS-1$ //$NON-NLS-2$
    }

    protected void updateLoaded(XModelObject o, File f) throws XModelException {
        FileSystemPeer peer = getFileSystem().getPeer();
        if(o instanceof FolderImpl) {
        	if(!o.getAttributeValue(XModelObjectConstants.ATTR_NAME).equals(f.getName())) {
        		o.setAttributeValue(XModelObjectConstants.ATTR_NAME, f.getName());
        		((FolderImpl)o).getResource();
        	}
            ((FolderImpl)o).update();
        } else {
            if(!registerFileInPeer(peer, f) && !isEncodingChanged(o)) {
            	if(!f.getName().equals(FileAnyImpl.toFileName(o))) {
            		String n = f.getName();
            		int i = n.lastIndexOf("."); //$NON-NLS-1$
            		String nm = (i >= 0) ? n.substring(0, i) : n;
            		String ext = (i >= 0) ? n.substring(i + 1) : "";  //$NON-NLS-1$
            		((FileAnyImpl)o).fileRenamed(nm, ext);
            	}
            	return;            
            }
			String p = FilePathHelper.toPathPath(f.getName());
            int i = (!o.isModified() || unsynchronized == null || !unsynchronized.contains(p)) ? 0 : question(f);
            if(i == 0) {
                reload(o, f);
            } else if(i == -100) {
            	final XModelObject o1 = o;
            	final File f1 = f;
            	Display.getDefault().asyncExec(new Runnable() {
            		public void run() {
            			if(question(f1) == 0) {
            				try {
            					reload(o1, f1);
            				} catch (XModelException e) {
            					ModelPlugin.getPluginLog().logError(e);
            				}
            			}
            		}
            	});
            }
        }
    }

    private boolean isEncodingChanged(XModelObject o) {
    	if(o != null && o.getModelEntity().getName().equals("FilePROPERTIES")) { //$NON-NLS-1$
    		String encoding = o.getAttributeValue("encoding"); //$NON-NLS-1$
    		if(encoding == null) {
    			return false;
    		}
    		String newEncoding = PropertiesLoader.getEncoding(o);
    		if(newEncoding == null) {
    			newEncoding = "8859_1"; //$NON-NLS-1$
    		}

    		if(!encoding.equals(newEncoding)) {
    			return true;
    		}
    	}
    	
    	return false;
    }
    private int question(File f) {
    	if(Display.getCurrent() == null) return -100;
		return getModel().getService().showDialog("Update",
				MessageFormat
						.format(
								"File {0} is externally modified.\nDo you want to reload it?",
								f.getAbsolutePath()), new String[]{"Yes", "No"}, null,
				ServiceDialog.QUESTION);
    }
    
    public void updateChildFile(XModelObject o, File f) throws XModelException {
		FileSystemPeer peer = getFileSystem().getPeer();
		if(!registerFileInPeer(peer, f)) return;
		int i = (!o.isModified()) ? 0 : 
			question(f);
		if(i == 0) {
			reload(o, f);
		}
    }
    
    public boolean isChangedEntity(XModelObject o, File f) {
		Properties p = getEntityProperties(f);
		return (!o.getModelEntity().getName().equals(p.getProperty(XMetaDataConstants.ENTITY)));
    }
    
    private void reload(XModelObject o, File f) throws XModelException {
        Properties p = getEntityProperties(f);
        if(!o.getModelEntity().getName().equals(p.getProperty(XMetaDataConstants.ENTITY))) {
            o.removeFromParent();
            createFileObject(f, p);
            return;
        }
        BodySource bs = getBodySource(f);
        if(isLateloadFile(o)) {
            FileAnyImpl impl = (FileAnyImpl)o;
            if(impl.getBodySource() != null) {
                impl.setBodySource(bs);
				fireObjectChanged(null);
            } else {
            	impl.edit(bs.get());
            	impl.setModified(false);
				XModelObjectLoaderUtil.updateModifiedOnSave(impl);
            }
        } else {
            XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(o);
            if(loader != null) {
                XModelObjectLoaderUtil.setTempBody(o, bs.get());
            	if("FilePROPERTIES".equals(o.getModelEntity().getName()) && bs instanceof EclipseFileBodySource) { //$NON-NLS-1$
            		String encoding = FileUtil.getEncoding(((EclipseFileBodySource)bs).ef);
            		if(encoding != null) o.setAttributeValue("encoding", encoding); //$NON-NLS-1$
            	}
                loader.update(o);
            } else if(o instanceof Reloadable) {
                ((Reloadable)o).reload();
            } else if(XModelObjectConstants.ENT_FILE_ANY_LONG.equals(o.getModelEntity().getName())) {
            	o.setModified(false);
            }
        }
    }

    protected boolean updateNew(String pathpart, File f, Map<String,XModelObject> toRemove) throws XModelException {
        FileSystemPeer peer = getFileSystem().getPeer();
        if(peer.contains(f) && !peer.isUpdated(f)) return false;
        XModelObject c = null;
        if(f.isDirectory()) {
            Properties p = new Properties();
            p.setProperty(XModelObjectConstants.ATTR_NAME, f.getName());
            c = getModel().createModelObject("FileFolder", p); //$NON-NLS-1$
			String pp = FilePathHelper.toPathPath(f.getName());
            if(linked.containsFile(pp)) {
            	c.setObject("file", linked.getFileByFileName(pp)); //$NON-NLS-1$
            }
        } else {
        	Properties ep = getEntityProperties(f);
        	XModelObject old = findOldObject(ep.getProperty(XMetaDataConstants.ENTITY), toRemove);
        	if(old != null) {
        		String ofn = FileAnyImpl.toFileName(old);
            	if(!f.getName().equals(ofn)) {
            		File of = new File(f.getParent(), ofn);
            		peer.unregister(of);
            		String nm = ep.getProperty(XModelObjectConstants.ATTR_NAME);
            		String ext = ep.getProperty(XModelObjectConstants.ATTR_NAME_EXTENSION);
            		((FileAnyImpl)old).fileRenamed(nm, ext);
            	}
        		updateLoaded(old, f);
        	} else {
        		createFileObject(f, ep);
        	}
        }
        peer.register(f);
        return (c != null && addChild(c));
    }
    
    private XModelObject findOldObject(String entity, Map<String,XModelObject> toRemove) {
    	if(entity == null || toRemove.size() == 0) return null;
    	Iterator<String> it = toRemove.keySet().iterator();
    	while(it.hasNext()) {
    		String nm = it.next().toString();
    		XModelObject o = (XModelObject)toRemove.get(nm);
    		if(o.getModelEntity().getName().equals(entity)) {
    			toRemove.remove(nm);
    			return o;
    		}
    	}
    	return null;
    }

    protected void updateRemove(XModelObject o) throws XModelException {
        boolean d = (o instanceof FolderImpl);
        FileSystemPeer peer = getFileSystem().getPeer();
        File rf = getChildIOFile(o);
        boolean c = (d && peer.containsDir(rf)) || ((!d) && peer.contains(rf));
        if(!c) return;
        int i = (!o.isModified()) ? 1 : question(o);
        if(i != 0) {
        	o.removeFromParent();
        } else {
			saveChild(o, peer, rf);
       		XActionInvoker.invoke("Open", o, null);  //$NON-NLS-1$
        }
        if(d) peer.unregisterDir(rf); else peer.unregister(rf);
    }
    
    public void removeChildFile(XModelObject o) {
		boolean d = (o instanceof FolderImpl);
		FileSystemPeer peer = getFileSystem().getPeer();
		File rf = getChildIOFile(o);
		boolean c = (d && peer.containsDir(rf)) || ((!d) && peer.contains(rf));
		if(!c) return;
		if(o.getModel().getModelBuffer().source() == o && rf.exists() && o.getModelEntity().getAttribute(XModelObjectConstants.ATTR_NAME__FILE) != null) {
			File temp = null;
			try {
				temp = File.createTempFile("efs_", rf.getName()); //$NON-NLS-1$
			} catch (IOException e) {
		    	  ModelPlugin.getPluginLog().logError(e);
			}
			if(temp != null) {
				FileUtil.copyFile(rf, temp);
				temp.deleteOnExit();
				o.set(XModelObjectConstants.ATTR_NAME__FILE, temp.getAbsolutePath());
			}
		}
		IResource r = (d) ? (IResource) getChildContainer(o.getAttributeValue(XModelObjectConstants.ATTR_NAME))
		                  : (IResource) getChildFile(FileAnyImpl.toFileName(o));
		o.removeFromParent();
		if(r.exists()) {
			try {
				r.delete(true, null);
			} catch (CoreException e) {
				ModelPlugin.getPluginLog().logError(e);
			}
		} else {
			rf.delete();
		}		
		if(d) peer.unregisterDir(rf); else peer.unregister(rf);
		XModelObjectLoaderUtil.updateModifiedFlag(this);
    }

    private static int question(XModelObject o) {
        return o.getModel().getService().showDialog("Update",
               MessageFormat
					.format(
							"File {0} is removed from the disk.\n Do you want to save your changes?",
							o.getModelEntity().getRenderer().getTitle(o)), new String[]{"Yes", "No"}, null,
               ServiceDialog.QUESTION);
    }

    private boolean fire = false;

    protected void fireStructureChanged(int kind, Object info) {
        if(fire) super.fireStructureChanged(kind, info);
    }

    public boolean hasChildren() {
        boolean q = super.hasChildren();
        if (q || loaded) return q;
        if(getParent() instanceof FolderImpl) {
        	FolderImpl p = (FolderImpl)getParent();
        	if(p.linked.containsFile(getPathPart())) return true;
        }
        File[] fs = getFiles();
        q = (fs != null && fs.length > 0);
        if(!q) loaded = fire = true;
        return q;
    }

    public boolean save() {
        File f = getFile();
        if(f == null) return true;
        if(f.exists() && !isModified()) return true;
        if(isOverlapped()) return true;
        boolean b = true;
        if(!f.exists()) {
        	IContainer c = getResource();
        	if(getFileType() == FOLDER && c instanceof IFolder) {
        		IFolder ef = (IFolder)c;
        		try {
        			ef.create(true, ef.getParent().isLocal(0), null);
        		} catch (CoreException e) {
        			ModelPlugin.getPluginLog().logError(e);
        		}
        	}
			if(!f.exists()) f.mkdirs();
        }
        File[] fs = getFiles();
        Map<String,File> t = new HashMap<String,File>();
        for (int i = 0; i < fs.length; i++) {
			String p = FilePathHelper.toPathPath(fs[i].getName());
        	t.put(p, fs[i]);
        }
        FileSystemPeer peer = getFileSystem().getPeer();
        peer.register(f);
        XModelObject[] cs = getChildren();
        for (int i = 0; i < cs.length; i++) {
            if(cs[i].getModelEntity().getName().equals(FileAuxiliary.AUX_FILE_ENTITY)) continue;
            if(cs[i] instanceof FolderLoader) {
                b &= ((FolderLoader)cs[i]).save();
            } else {
            	File d = linked.getFileByFileName(cs[i].getPathPart());
            	if(d == null) {
                    d = new File(f, FileAnyImpl.toFileName(cs[i]));
            	}
            	try {
            		b &= saveChild(cs[i], peer, d);
            	} catch (XModelException ee) {
            		//TODO maybe it should be rethrown
            		ModelPlugin.getPluginLog().logError(ee);
            	}
            }
            t.remove(cs[i].getPathPart());
        }

        cs = getChildren(FileAuxiliary.AUX_FILE_ENTITY);
        for (int i = 0; i < cs.length; i++) {
            File cf = getChildIOFile(cs[i]);
            if(!cf.exists()) continue;
            peer.register(cf);
            FileAnyImpl impl = (FileAnyImpl)cs[i];
            if(impl.getBodySource() == null) impl.setBodySource(getBodySource(cf));
            t.remove(cs[i].getPathPart());
        }

        Iterator<File> it = t.values().iterator();
        while(it.hasNext()) {
            File df = it.next();
            boolean d = df.isDirectory();
            boolean r = (d && peer.containsDir(df)) || ((!d) && peer.contains(df));
            if(!r) continue;
            XModelObjectLoaderUtil.remove(df);
            if(d) peer.unregisterDir(df); else peer.unregister(df);
        }
        if(b) setModified(false);
        return b;
    }

    public boolean saveChild(XModelObject c) throws XModelException {
        if(c == null || c.getParent() != this) return false;
        if(!c.isModified()) return true;
        File folder = getFile();
        if(folder==null || !folder.exists()) return save();
        updateLock++;
        boolean b = false;
        try {
        	File d = linked.getFileByFileName(c.getPathPart());
        	if(d == null) {
                d = new File(folder, FileAnyImpl.toFileName(c));
        	}
			saveChild(c, getFileSystem().getPeer(), d);
			if(b) XModelObjectLoaderUtil.updateModifiedOnSave(c);
        } finally {
			updateLock--;
        } 
        update();  // on action
		ResourceMarkers.refreshProblemMarkersAsync(c);
        return b;
    }

    private boolean saveChild(XModelObject c, FileSystemPeer peer, File cf) throws XModelException {
        boolean b = true;
        if(!cf.exists()) c.setModified(true);
        if(!c.isModified()) return true;
        XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(c);
        if(loader != null) b &= loader.save(c);
        BodySource bs = getBodySource(cf);
        boolean h = (loader == null) || bs.write(c);
        if(loader == null) saveFileWithoutLoader(cf, c);
        if(h && c.isModified() || isChangedEntity(c, cf)) reload(c, cf);
        b &= h;
        peer.register(cf);
        if(isLateloadFile(c)) {
            FileAnyImpl impl = (FileAnyImpl)c;
            if(impl.getBodySource() == null) impl.setBodySource(bs);
        }
        return b;
    }
    
    /**
     * Used only by ObjectMultiPageEditor to prevent 
     * reload after saveX method. This is a precaution 
     * for the case when file update job may fail.
     * @param child
     */
    
    public void updateRegistration(XModelObject child) {
        if(child == null || child.getParent() != this) return;
        File folder = getFile();
        if(folder == null || !folder.exists() || getFileSystem() == null) return;
        FileSystemPeer peer = getFileSystem().getPeer();
        File cf = new File(folder, FileAnyImpl.toFileName(child));
        if(!cf.exists()) child.setModified(true);
        if(child.isModified()) return;
        peer.register(cf);
    }

    private boolean saveFileWithoutLoader(File f, XModelObject o) {
        if(!o.isModified()) return true;
        if(o.getModelEntity().getAttribute(XModelObjectConstants.ATTR_NAME__FILE) == null) return true;
        String sfn = o.get(XModelObjectConstants.ATTR_NAME__FILE);
        if(sfn.length() == 0) return true;
        if(f.getAbsolutePath().equalsIgnoreCase(sfn)) {
            o.setModified(false);
        	return true;
        }
        File sf = new File(sfn);
        if(sf.isFile()) {
        	f.getParentFile().mkdirs();
        	FileUtil.copyFile(sf, f);
        }
        o.set(XModelObjectConstants.ATTR_NAME__FILE, f.getAbsolutePath());
        o.setModified(false);
        return true;
    }

    public boolean changeChildTimeStamp(XModelObject c) throws XModelException {
        if(c == null || c.getParent() != this) return false;
        File cf = new File(getFile(), FileAnyImpl.toFileName(c));
        if(!cf.exists()) return saveChild(c);
        long s = cf.lastModified();
        if(c.isModified()) {
            saveChild(c);
            if(s != cf.lastModified()) return true;
        }
        cf.setLastModified(System.currentTimeMillis());
        getFileSystem().getPeer().register(cf);
        return true;
    }
    
    public void discardChildFile(XModelObject c) throws XModelException {
    	if(c == null || !c.isActive() || !c.isModified() || c.getParent() != this) return;
    	c.setModified(false);
		XModelObjectLoaderUtil.updateModifiedOnSave(c);
    	File folder = getFile();
    	if(!folder.exists()) return;
		File cf = new File(folder, FileAnyImpl.toFileName(c));
		String path = c.getPath();
        setForceLoadProperty(c, true);
		reload(c, cf);
        setForceLoadProperty(c, false);
		c = getModel().getByPath(path);
		if(c != null) c.getChildren();
    	
    }

    public static void parseFileName(Properties p, String fn) {
        int i = fn.lastIndexOf('.');
        String n = (i < 0) ? fn : fn.substring(0, i);
        String e = (i < 0) ? "" : fn.substring(i + 1); //$NON-NLS-1$
        p.setProperty(XModelObjectConstants.ATTR_NAME, n);
        p.setProperty(XModelObjectConstants.ATTR_NAME_EXTENSION, e);
    }

    public String getPathPart() {
        String s = get(XModelObjectConstants.XML_ATTR_NAME);
        return FilePathHelper.toPathPath(s);
    }

    public XModelObject getChildByPathPart(String pathpart) {
    	if(linked.filesByLinkName.containsKey(pathpart)) {
    		File f = linked.getFileByResourceName(pathpart);
    		pathpart = f.getName();
    	}
    	pathpart = FilePathHelper.toPathPath(pathpart);
        return super.getChildByPathPart(pathpart);
    }

    static boolean isLateloadFile(XModelObject o) {
        return (o.getModelEntity().getAttribute("_lateload") != null && //$NON-NLS-1$
                o.getModelEntity().getAttribute(XModelObjectConstants.ATTR_NAME_BODY) != null);
    }

    static boolean isLateloadFile2(XModelObject o) {
        return (o.getModelEntity().getAttribute("_lateload") != null); //$NON-NLS-1$
    }

    protected void copy_children(XModelObject copy, boolean transform) {
        super.copy_children(copy, transform);
        if(copy instanceof FolderImpl) {
            FolderImpl f = (FolderImpl)copy;
            f.loaded = true;
            f.fire = true;
        }
    }

    private static int editability = -1;

    private void initEditability() {
        if(editability > -1) return;
            editability = (null != getModelEntity().getActionList().getItem("DeleteActions").getItem("Delete")) //$NON-NLS-1$ //$NON-NLS-2$
                          ? 1 : 0;
    }

    public boolean isObjectEditable() {
        initEditability();
        return (editability == 1 && !XModelObjectConstants.TRUE.equals(get("overlapped")) && isActive()); //$NON-NLS-1$
    }

    public String getMainIconName() {
        if(XModelObjectConstants.TRUE.equals(get("overlapped")) && isActive()) { //$NON-NLS-1$
          String oin = get("overlappedSystem"); //$NON-NLS-1$
          XModelObject o = (oin == null || oin.length() == 0) ? null : getModel().getByPath(oin);
          if(o == null) {
              o = this;
              while(o != null && o.getFileType() != XFileObject.SYSTEM) o = o.getParent();
          }
          if(o != null && o != this) return o.getMainIconName();
        }
        return super.getMainIconName();
    }

    public IContainer getChildContainer(String name) {
		if(linked.containsFile(name)) {
			IResource r = linked.getResourceByFileName(name);
			return r instanceof IContainer ? (IContainer)r : null;
		}
    	IContainer c = getResource();
    	return (c == null) ? null : c.getFolder(new Path(XModelObjectConstants.SEPARATOR + name));
    }
    
	public IFile getChildFile(String name) {
		if(linked.containsFile(name)) {
			IResource r = linked.getResourceByFileName(name);
			return r instanceof IFile ? (IFile)r : null;
		}
    	IContainer c = getResource();
		return (c == null) ? null : c.getFile(new Path(XModelObjectConstants.SEPARATOR + name));
	}

	public IContainer getResource() {
		if(!needUpdateResource()) return resource;
		resource = ((FolderImpl)getParent()).getChildContainer(getAttributeValue(XModelObjectConstants.ATTR_NAME));
		pathForResource = getPath();
		return resource;
	}
	
	protected boolean needUpdateResource() {
		if(resource != null && !resource.getName().equals(getAttributeValue(XModelObjectConstants.ATTR_NAME))) return true;
		if(!isActive()) return false;
		if(pathForResource == null || resource == null) return true;
		String path = getPath();
		return (path != null && !path.equals(pathForResource));
	}
	
	public IResource[] getResources() {
		try {
			if(!isActive()) return null;
	    	IContainer c = getResource();
			return (c != null && c.isAccessible()) ? c.members() : new IResource[0];
		} catch (CoreException e) {
			ModelPlugin.getPluginLog().logError(e);
			return new IResource[0];  
		}
	}    

	public boolean isOverlapped() {
		XModelObject p = this;
		while(p != null && !XModelObjectConstants.TRUE.equals(get("overlapped"))) p = p.getParent(); //$NON-NLS-1$
		return p != null;
	}

	public Object getAdapter(Class adapter) {
		if(IResource.class == adapter) return getResource();
		return super.getAdapter(adapter);
	}

	public boolean isRemoved() {
		return resource != null && !resource.exists();
	}

}

class FileBodySource implements BodySource {
    private File f = null;

    public FileBodySource(File f) {
        this.f = f;
    }

    public String get() {
    	String encoding = ResourcesPlugin.getEncoding();
		return FileUtil.readFileWithEncodingCheck(f, encoding);
    }

    public boolean write(Object object) {
        if(!(object instanceof XModelObject)) return false;
        XModelObject o = (XModelObject)object;
        
        boolean b = XModelObjectLoaderUtil.saveBody(f, o, ResourcesPlugin.getEncoding());

		XModelObject p = o.getParent();
		while(p != null && p.getFileType() < XFileObject.FOLDER) p = p.getParent();
		if(p instanceof FolderImpl) {
			((FolderImpl)p).getFileSystem().getPeer().register(f);
		}

		return b;
    }

}

class LinkedResources {
	protected Map<String, File> filesByLinkName = new HashMap<String, File>();
	protected Map<String, File> filesByFileName = new HashMap<String, File>();
	protected Map<String, IResource> resourcesByLinkName = new HashMap<String, IResource>();
	protected Map<String, IResource> resourcesByFileName = new HashMap<String, IResource>();

	public boolean containsFile(String name) {
		if(filesByFileName.containsKey(name)) return true;
		return false;
	}

	public File getFileByResourceName(String name) {
		return filesByLinkName.get(name);
	}

	public File getFileByFileName(String name) {
		return filesByFileName.get(name);
	}

	public IResource getResourceByResourceName(String name) {
		return resourcesByLinkName.get(name);
	}

	public IResource getResourceByFileName(String name) {
		return resourcesByFileName.get(name);
	}

	public void registerResource(IResource r) {
    	File f = r.getLocation().toFile();
        String pp = FilePathHelper.toPathPath(f.getName());
        filesByFileName.put(pp, f);
        filesByLinkName.put(r.getName(), f);
        resourcesByFileName.put(pp, r);
        resourcesByLinkName.put(r.getName(), r);
	}

	public void clearFiles() {
		filesByFileName.clear();
		filesByLinkName.clear();
	}
}

class EclipseFileBodySource implements BodySource {
	IFile ef = null;
	File f;

	public EclipseFileBodySource(IFile ef, File f) {
		this.ef = ef;
		this.f = f;
	}

	public String get() {
		String encoding = null;
		if (ef != null && ef.exists()) {
			encoding = FileUtil.getEncoding(ef);
		}
		if (encoding == null) {
			encoding = ResourcesPlugin.getEncoding();
		}
		try {
			boolean isUTF8BOM = ModelPlugin.isUTF8BOM(encoding, ef);
			if (!isUTF8BOM) {
				return FileUtil.readFileWithEncodingCheck(f, encoding);
			} else {
				return ModelPlugin.getContent(ef.getContents(), encoding, true);
			}
		} catch (CoreException e) {
			IStatus status = new Status(IStatus.ERROR, ModelPlugin.PLUGIN_ID, IStatus.OK,e.getLocalizedMessage(), e);
			ModelPlugin.getDefault().getLog().log(status);
			return null;
		}
	}

	public boolean write(Object object) {
		if(!(object instanceof XModelObject)) return false;
		XModelObject o = (XModelObject)object;
		try {
			String encoding = null;
			if(ef != null && ef.exists()) {
				encoding = FileUtil.getEncoding(ef);
			}
			if(encoding == null) {
				encoding = ResourcesPlugin.getEncoding();
			}
			XModelObjectLoaderUtil.saveBody(f, o, encoding);
			/*
			String r = XModelObjectLoaderUtil.getTempBody(o);
			ByteArrayInputStream is = new ByteArrayInputStream(r.getBytes());
			if(ef.exists()) ef.setContents(is, true, false, null);
			else ef.create(is, true, null);
			*/ 
			o.setModified(false);
			XModelObject p = o.getParent();
			while(p != null && p.getFileType() < XFileObject.FOLDER) p = p.getParent();
			if(p instanceof FolderImpl) {
				((FolderImpl)p).getFileSystem().getPeer().register(f);
			}
			ef.refreshLocal(IFile.DEPTH_INFINITE, null);
		} catch (CoreException e) {   
			ModelPlugin.getPluginLog().logError(e);
		}
		return true;
	}
	
}
