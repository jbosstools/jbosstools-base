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
import java.util.*;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Path;
import org.jboss.tools.common.model.markers.ResourceMarkers;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.eclipse.swt.widgets.Display;

import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.ServiceDialog;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.BodySource;
import org.jboss.tools.common.model.filesystems.FileAuxiliary;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.impl.RegularObjectImpl;
import org.jboss.tools.common.model.impl.XModelImpl;
import org.jboss.tools.common.model.loaders.Reloadable;
import org.jboss.tools.common.model.loaders.XObjectLoader;
import org.jboss.tools.common.model.util.Paths;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;
import org.jboss.tools.common.util.FileUtil;

public class FolderImpl extends RegularObjectImpl implements FolderLoader {
    private static final long serialVersionUID = 8082046187736790127L;
    protected boolean loaded = false;
	protected IContainer resource = null;
	protected String pathForResource = null;

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
        File f = getFile();
        if(f == null) return true;
        f = new File(f, o.getPathPart());
        return (!f.isFile() || f.canWrite());
    }

    protected File getFile() {
        String path = getAbsolutePath();
        return (path == null) ? null :
               new File(Paths.expand(path, getModel().getProperties()));
    }

    public BodySource getBodySource(String filename) {
        File f = getFile();
        return (f == null) ? null : getBodySource(new File(f, filename));
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
        String p = (getParent() == null) ? null : ((FolderImpl)getParent()).getAbsolutePath();
        return (p == null) ? null : p + "/" + name();
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
        if("NAME".equals(name) && isActive()) {
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
        Properties p = new Properties();
        for (int i = 0; i < fs.length; i++) {
            if(fs[i].isDirectory()) {
                p.clear();
                p.setProperty("name", fs[i].getName());
                XModelObject c = getModel().createModelObject("FileFolder", p);
                addChild(c);
            } else {
                createFileObject(fs[i]);
            }
            peer.register(fs[i]);
        }
        fire = true;
    }

    private Properties getEntityProperties(File f) {
        Properties p = new Properties();
        parseFileName(p, f.getName());
        String ext = p.getProperty("extension");
        String body = null;
        String entity = getModel().getEntityRecognizer().getEntityName(ext, body);
        if("FileAny".equals(entity)) {
        	boolean isText = XModelObjectLoaderUtil.isTextFile(f, 100);
            if(f.length() > 100000 || !isText) entity = "FileAnyLong";
            else if(isText) entity = "FileTXT";
        } else if(entity == null) {
            body = getBodySource(f).get();
            entity = getModel().getEntityRecognizer().getEntityName(ext, body);
        }
        if(entity == null || getModel().getMetaData().getEntity(entity) == null) entity = "FileAny";
        p.setProperty("entity", entity);
        if(body != null) p.setProperty("body", body);
        return p;
    }

    private void createFileObject(File f) {
        createFileObject(f, getEntityProperties(f));
    }

    private void createFileObject(File f, Properties p) {
    	BodySource bs = getBodySource(f);
        String body = p.getProperty("body");
        String entity = p.getProperty("entity");
        XModelObject c = getModel().createModelObject(entity, p);
        if(c == null) {
        	ModelPlugin.getPluginLog().logInfo("Cannot create file for entity " + entity);
        	return;
        }
        if(isLateloadFile2(c)) {
            ((FileAnyImpl)c).setBodySource(bs);
        } else {
            XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(c);
            if(loader != null) {
                if(body == null) body = bs.get();
                XModelObjectLoaderUtil.setTempBody(c, body);
                loader.load(c);
            } else if(c.getModelEntity().getAttribute("_file") != null) {
                c.set("_file", f.getAbsolutePath());
            }
        }
        addChild(c);
    }

	int updateLock = 0;
	Set<String> unsynchronized = null;
    public boolean update() {
        if(!loaded) return true;
        if(updateLock > 0) return true;
        updateLock++;
      try {  
		FileSystemsImpl fsi = (FileSystemsImpl)getFileSystem().getParent();
        if(resource != null && resource.exists() && !resource.isSynchronized(IResource.DEPTH_ONE)) try	{
			fsi.lockUpdate();
			resource.refreshLocal(IResource.DEPTH_ZERO, null);
			IResource[] rs = resource.members();
			for (int i = 0; i < rs.length; i++) {
				if(!rs[i].isSynchronized(IResource.DEPTH_ZERO)) {
					if(unsynchronized == null) unsynchronized = new HashSet<String>();
					unsynchronized.add(rs[i].getName().toLowerCase());
				}
			}
			resource.refreshLocal(IResource.DEPTH_ONE, null);
		} catch (Exception e) {
			ModelPlugin.getPluginLog().logError("Exception caught in FolderImpl.update()");
		} finally {
			fsi.unlockUpdate();
		}
        File[] fs = getFiles();
        Map<String,File> mf = new HashMap<String,File>();
        for (int i = 0; i < fs.length; i++) mf.put(fs[i].getName().toLowerCase(), fs[i]);

        Map<String,XModelObject> mc = children.getObjectsMap();

        updateAuxiliary(mc, mf);

        Map<String,XModelObject> toRemove = new HashMap<String,XModelObject>();
        Iterator io = mc.keySet().iterator();
        while(io.hasNext()) {
            String nm = (String)io.next();
            if(mf.containsKey(nm)) continue;
            XModelObject o = (XModelObject)mc.get(nm);
            String s = FileAnyImpl.toFileName(o);
            File of = new File(getFile(), s);
            if(o.getFileType() == XModelObject.FOLDER) {
                if(!getFileSystem().getPeer().containsDir(of)) continue;
            } else {
                if(!getFileSystem().getPeer().contains(of)) continue;
            }
            toRemove.put(nm, o);
            io.remove();
        }
        
        Iterator it = mf.keySet().iterator();
        while(it.hasNext()) {
            String nm = (String)it.next();
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
      } catch (NoClassDefFoundError error) {
    	  //Most probably Eclipse is shutting down.
    	  return true;
      } catch (Exception t) {
    	  ModelPlugin.getPluginLog().logError("Exception caught in FolderImpl.update()");
      } finally {  
		updateLock--;
		unsynchronized = null;
      }
        return true;
    }

    private void updateAuxiliary(Map mc, Map mf) {
        Iterator it = mf.keySet().iterator();
        while(it.hasNext()) {
            String nm = (String)it.next();
            File f = (File)mf.get(nm);
            XModelObject o = (XModelObject)mc.get(nm);
            if(o == null || !o.getModelEntity().getName().equals(FileAuxiliary.AUX_FILE_ENTITY)) continue;
            it.remove();
            FileAnyAuxiliaryImpl aux = (FileAnyAuxiliaryImpl)o;
            mc.remove(nm);
            if(aux.isObsolete()) {
                FileAuxiliary h =  aux.getAuxiliaryHelper();
                XModelObject main = aux.getMainObject();
                if(main != null && main.isActive() && h != null) {
                	String n = h.getAuxiliaryName(main);
                	String p = n + "." + aux.getAttributeValue("extension");
                	XModelObject other = getChildByPath(p);
                	if(other != null && other != aux) other.removeFromParent();                	
                	aux.fileRenamed(n, aux.getAttributeValue("extension"));
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
                   					} catch (Exception e) {
                   						//ignore
                   					}
                   				}
                   				ef = getChildFile(r.getName());
                   				if(ef != null && !ef.isSynchronized(0)) {
                   					try {
                   						ef.refreshLocal(0, null);
                   					} catch (Exception e) {
                   						//ignore
                   					}
                   				}
                   				
                   			} else {
                   				f.delete();
                                getFileSystem().getPeer().unregister(f);
                   				IFile ef = getChildFile(f.getName());
                   				if(ef != null && !ef.isSynchronized(0)) {
                   					try {
                   						ef.refreshLocal(0, null);
                   					} catch (Exception e) {
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
    	f.set("forceLoad", b ? "true" : "");    	
    }

    protected void updateLoaded(XModelObject o, File f) {
        FileSystemPeer peer = getFileSystem().getPeer();
        if(o instanceof FolderImpl) {
        	if(!o.getAttributeValue("name").equals(f.getName())) {
        		o.setAttributeValue("name", f.getName());
        		((FolderImpl)o).getResource();
        	}
            ((FolderImpl)o).update();
        } else {
            if(!registerFileInPeer(peer, f)) {
            	if(!f.getName().equals(FileAnyImpl.toFileName(o))) {
            		String n = f.getName();
            		int i = n.lastIndexOf(".");
            		String nm = (i >= 0) ? n.substring(0, i) : n;
            		String ext = (i >= 0) ? n.substring(i + 1) : ""; 
            		((FileAnyImpl)o).fileRenamed(nm, ext);
            	}
            	return;            
            }
            int i = (!o.isModified() || unsynchronized == null || !unsynchronized.contains(f.getName().toLowerCase())) ? 0 : question(f);
            if(i == 0) {
                reload(o, f);
            } else if(i == -100) {
            	final XModelObject o1 = o;
            	final File f1 = f;
            	Display.getDefault().asyncExec(new Runnable() {
            		public void run() {
            			if(question(f1) == 0) reload(o1, f1);
            		}
            	});
            }
        }
    }
    
    private int question(File f) {
    	if(Display.getCurrent() == null) return -100;
    	try {
    		return getModel().getService().showDialog("Update",
    				"File " + f.getAbsolutePath() + " is externally modified.\n" +
    				"Do you want to reload it?", new String[]{"Yes", "No"}, null,
    				ServiceDialog.QUESTION);
    	} catch (Exception t) {
    		return 0;
    	}
    }
    
    public void updateChildFile(XModelObject o, File f) {
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
		return (!o.getModelEntity().getName().equals(p.getProperty("entity")));
    }
    
    private void reload(XModelObject o, File f) {
        Properties p = getEntityProperties(f);
        if(!o.getModelEntity().getName().equals(p.getProperty("entity"))) {
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
                loader.update(o);
            } else if(o instanceof Reloadable) {
                ((Reloadable)o).reload();
            } else if("FileAnyLong".equals(o.getModelEntity().getName())) {
            	o.setModified(false);
            }
        }
    }

    protected boolean updateNew(String pathpart, File f, Map toRemove) {
        FileSystemPeer peer = getFileSystem().getPeer();
        if(peer.contains(f) && !peer.isUpdated(f)) return false;
        XModelObject c = null;
        if(f.isDirectory()) {
            Properties p = new Properties();
            p.setProperty("name", f.getName());
            c = getModel().createModelObject("FileFolder", p);
        } else {
        	Properties ep = getEntityProperties(f);
        	XModelObject old = findOldObject(ep.getProperty("entity"), toRemove);
        	if(old != null) {
        		String ofn = FileAnyImpl.toFileName(old);
            	if(!f.getName().equals(ofn)) {
            		File of = new File(f.getParent(), ofn);
            		peer.unregister(of);
            		String nm = ep.getProperty("name");
            		String ext = ep.getProperty("extension");
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
    
    private XModelObject findOldObject(String entity, Map toRemove) {
    	if(entity == null || toRemove.size() == 0) return null;
    	Iterator it = toRemove.keySet().iterator();
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

    protected void updateRemove(XModelObject o) {
        boolean d = (o instanceof FolderImpl);
        FileSystemPeer peer = getFileSystem().getPeer();
        File f = getFile();
        String p = o.getPathPart();
        File rf = new File(f, p);
        boolean c = (d && peer.containsDir(rf)) || ((!d) && peer.contains(rf));
        if(!c) return;
        int i = (!o.isModified()) ? 1 : question(o);
        if(i != 0) {
        	o.removeFromParent();
        } else {
			saveChild(o, peer, f);
       		XActionInvoker.invoke("Open", o, null); 
        }
        if(d) peer.unregisterDir(rf); else peer.unregister(rf);
    }
    
    public void removeChildFile(XModelObject o) {
		boolean d = (o instanceof FolderImpl);
		FileSystemPeer peer = getFileSystem().getPeer();
		File f = getFile();
		String p = o.getPathPart();
		File rf = new File(f, p);
		boolean c = (d && peer.containsDir(rf)) || ((!d) && peer.contains(rf));
		if(!c) return;
		if(o.getModel().getModelBuffer().source() == o && rf.exists() && o.getModelEntity().getAttribute("_file") != null) {
			File temp = null;
			try {
				temp = File.createTempFile("efs_", rf.getName());
			} catch (Exception e) {
				//ignore
			}
			if(temp != null) {
				FileUtil.copyFile(rf, temp);
				temp.deleteOnExit();
				o.set("_file", temp.getAbsolutePath());
			}
		}
		IResource r = (d) ? (IResource) getChildContainer(o.getAttributeValue("name"))
		                  : (IResource) getChildFile(FileAnyImpl.toFileName(o));
		o.removeFromParent();
		if(r.exists()) {
			try {
				r.delete(true, null);
			} catch (Exception e) {
				//ignore
			}
		} else {
			rf.delete();
		}		
		if(d) peer.unregisterDir(rf); else peer.unregister(rf);
		XModelObjectLoaderUtil.updateModifiedFlag(this);
    }

    private static int question(XModelObject o) {
        String t = "File" + " " + o.getModelEntity().getRenderer().getTitle(o);
        try {
        	return o.getModel().getService().showDialog("Update",
               t + " is removed from the disk.\n " +
               "Do you want to save your changes?", new String[]{"Yes", "No"}, null,
               ServiceDialog.QUESTION);
        } catch (Exception e) {
        	return 0;
        }
    }

    private boolean fire = false;

    protected void fireStructureChanged(int kind, Object info) {
        if(fire) super.fireStructureChanged(kind, info);
    }

    public boolean hasChildren() {
        boolean q = super.hasChildren();
        if (q || loaded) return q;
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
        		} catch (Exception e) {
        			//ignore
        		}
        	}
			if(!f.exists()) f.mkdirs();
        }
        File[] fs = getFiles();
        Map<String,File> t = new HashMap<String,File>();
        for (int i = 0; i < fs.length; i++) t.put(fs[i].getName().toLowerCase(), fs[i]);
        FileSystemPeer peer = getFileSystem().getPeer();
        peer.register(f);
        XModelObject[] cs = getChildren();
        for (int i = 0; i < cs.length; i++) {
            if(cs[i].getModelEntity().getName().equals(FileAuxiliary.AUX_FILE_ENTITY)) continue;
            if(cs[i] instanceof FolderLoader) {
                b &= ((FolderLoader)cs[i]).save();
            } else {
                b &= saveChild(cs[i], peer, f);
            }
            t.remove(cs[i].getPathPart());
        }

        cs = getChildren(FileAuxiliary.AUX_FILE_ENTITY);
        for (int i = 0; i < cs.length; i++) {
            File cf = new File(f, FileAnyImpl.toFileName(cs[i]));
            if(!cf.exists()) continue;
            peer.register(cf);
            FileAnyImpl impl = (FileAnyImpl)cs[i];
            if(impl.getBodySource() == null) impl.setBodySource(getBodySource(cf));
            t.remove(cs[i].getPathPart());
        }

        Iterator it = t.values().iterator();
        while(it.hasNext()) {
            File df = (File)it.next();
            boolean d = df.isDirectory();
            boolean r = (d && peer.containsDir(df)) || ((!d) && peer.contains(df));
            if(!r) continue;
            XModelObjectLoaderUtil.remove(df);
            if(d) peer.unregisterDir(df); else peer.unregister(df);
        }
        if(b) setModified(false);
        return b;
    }

    public boolean saveChild(XModelObject c) {
        if(c == null || c.getParent() != this) return false;
        if(!c.isModified()) return true;
        File folder = getFile();
        if(folder==null || !folder.exists()) return save();
        updateLock++;
        boolean b = false;
        try {
			saveChild(c, getFileSystem().getPeer(), folder);
			if(b) XModelObjectLoaderUtil.updateModifiedOnSave(c);
        } finally {
			updateLock--;
        } 
        update();  // on action
		ResourceMarkers.refreshProblemMarkersAsync(c);
        return b;
    }

    private boolean saveChild(XModelObject c, FileSystemPeer peer, File folder) {
        boolean b = true;
        File cf = new File(folder, FileAnyImpl.toFileName(c));
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
        if(o.getModelEntity().getAttribute("_file") == null) return true;
        String sfn = o.get("_file");
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
        o.set("_file", f.getAbsolutePath());
        o.setModified(false);
        return true;
    }

    public boolean changeChildTimeStamp(XModelObject c) {
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
    
    public void discardChildFile(XModelObject c) {
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
        String e = (i < 0) ? "" : fn.substring(i + 1);
        p.setProperty("name", n);
        p.setProperty("extension", e);
    }

    public String getPathPart() {
        String s = get("NAME");
        return (s == null) ? null : s.toLowerCase();
    }

    public XModelObject getChildByPathPart(String pathpart) {
        return super.getChildByPathPart(pathpart.toLowerCase());
    }

    static boolean isLateloadFile(XModelObject o) {
        return (o.getModelEntity().getAttribute("_lateload") != null &&
                o.getModelEntity().getAttribute("body") != null);
    }

    static boolean isLateloadFile2(XModelObject o) {
        return (o.getModelEntity().getAttribute("_lateload") != null);
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
        try {
            editability = (null != getModelEntity().getActionList().getItem("DeleteActions").getItem("Delete"))
                          ? 1 : 0;
        } catch (Exception e) {
            editability = 0;
        }
    }

    public boolean isObjectEditable() {
        initEditability();
        return (editability == 1 && !"true".equals(get("overlapped")) && isActive());
    }

    public String getMainIconName() {
        if("true".equals(get("overlapped")) && isActive()) {
          String oin = get("overlappedSystem");
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
    	IContainer c = getResource();
    	return (c == null) ? null : c.getFolder(new Path("/" + name));
    }
    
	public IFile getChildFile(String name) {
    	IContainer c = getResource();
		return (c == null) ? null : c.getFile(new Path("/" + name));
	}

	public IContainer getResource() {
		if(!needUpdateResource()) return resource;
		resource = ((FolderImpl)getParent()).getChildContainer(getAttributeValue("name"));
		pathForResource = getPath();
		return resource;
	}
	
	protected boolean needUpdateResource() {
		if(resource != null && !resource.getName().equals(getAttributeValue("name"))) return true;
		if(!isActive()) return false;
		if(pathForResource == null || resource == null) return true;
		String path = getPath();
		return (path != null && !path.equals(pathForResource));
	}
	
	public IResource[] getResources() {
		try {
			if(!isActive()) return null;
	    	IContainer c = getResource();
			return (c != null) ? c.members() : new IResource[0];
		} catch (Exception e) {
			return new IResource[0];  
		}
	}    

	public boolean isOverlapped() {
		XModelObject p = this;
		while(p != null && !"true".equals(get("overlapped"))) p = p.getParent();
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
        return XModelObjectLoaderUtil.readFile(f);
    }

    public boolean write(Object object) {
        if(!(object instanceof XModelObject)) return false;
        XModelObject o = (XModelObject)object;
        return XModelObjectLoaderUtil.saveBody(f, o);
    }

}

class EclipseFileBodySource implements BodySource {
	private IFile ef = null;
	private File f;

	public EclipseFileBodySource(IFile ef, File f) {
		this.ef = ef;
		this.f = f;
	}

	public String get() {
		try {
			return XModelObjectLoaderUtil.readFile(f);
///			return FileUtil.readStream(ef.getContents());
		} catch (Exception e) {
			return "";
		}
	}

	public boolean write(Object object) {
		if(!(object instanceof XModelObject)) return false;
		XModelObject o = (XModelObject)object;
		try {
			XModelObjectLoaderUtil.saveBody(f, o);
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
		} catch (Exception e) {}
		return true;
	}
	
}
