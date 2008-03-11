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

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.swt.widgets.Display;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.constraint.XProperty;
import org.jboss.tools.common.model.XJob.XRunnable;
import org.jboss.tools.common.model.filesystems.*;

public class FileAnyImpl extends RegularObjectImpl {
	/**
	 * This is text editor that reads text from file than 
	 * from model object. If some action modifies text
	 * by getModel().editObjectAttribute() method, text 
	 * editor must be notified.
	 */
	public interface BodyListener {
		public void bodyChanged(String body);
	}

    private static final long serialVersionUID = 5613864065417244573L;
    private BodySource bodysource = null;
    BodyListener listener;
    
    int updateLock = 0;
    
    public void setUpdateLock() {
    	updateLock++;
    }
    
    public void releaseUpdateLock() {
    	updateLock--;
    }

    public FileAnyImpl() {}
    
    public void addListener(BodyListener listener) {
    	this.listener = listener;
    }

    public void removeListener(BodyListener listener) {
    	if(this.listener == listener) this.listener = null;
    }

    public void setModified(boolean value) {
        boolean changed = (isModified() != value);
    	if(changed && value) {
    		//The layout of process files will not be saved!
    		XModelObject p = getParent();
    		if(p instanceof JarFolderImpl) return;
    	}
        super.setModified(value);
        if(changed && isActive() && updateLock <= 0) {
        	fireObjectChanged("setModified");
        	if(value) {
        		saveUnopen();
        	}
        } 
    }

    public boolean isObjectEditable() {
        return isObjectEditable0();
    }
    
    private boolean isObjectEditable0() {
		if("true".equals(get("overlapped")) && isActive()) return false;
		XModelObject p = getParent();
		if(p == null) return true;
		if(p instanceof JarFolderImpl) return false;
		if(p instanceof FolderImpl) return ((FolderImpl)p).isChildEditable(this);
		return true;
	}

	public boolean isAttributeEditable(String name) {
		if("name".equals(name) || "extension".equals(name)) return false;
		if("body".equals(name)) {
			return isObjectEditable0();
		}
		return super.isAttributeEditable(name);
	}

    public String getMainIconName() {
        if("true".equals(get("overlapped")) && isActive()) {
          String oin = get("overlappedSystem");
          XModelObject o = (oin == null || oin.length() == 0) ? null : getModel().getByPath(oin);
          if(o == null) {
              o = this;
              while(o != null && o.getFileType() != XFileObject.SYSTEM) o = o.getParent();
          }
          if(o != null) return o.getMainIconName();
        }
        return super.getMainIconName();
    }

    public String name() {
        return toFileName(this);
    }

    public int getFileType() {
        return FILE;
    }

    public String getPathPart() {
        String n = name();
        return (n == null) ? null : n.toLowerCase();
    }

    public static String toFileName(XProperty p) {
        String n = p.get("NAME");
        if(n == null) return null;
        String s = p.get("EXTENSION");
        return n + ((s == null || s.length() == 0) ? "" : "." + s);
    }

    public void setBodySource(BodySource bodysource) {
        this.bodysource = bodysource;
        super.set("body", "");
        changeTimeStamp();
    }

    public void updateBodySource() {
        XModelObject o = getParent();
        if(!(o instanceof FolderLoader)) return;
        FolderLoader f = (FolderLoader)o;
        bodysource = f.getBodySource(toFileName(this));
    }

    public BodySource getBodySource() {
        return bodysource;
    }

    public String get(String name) {
        if(bodysource != null && "body".equals(name)) readBodySource();
        return super.get(name);
    }

    private void readBodySource() {
        String s = bodysource.get();
        bodysource = null;
        if(s != null) super.set("body", s);
    }

    public String setAttributeValue(String name, String value) {
//        if("extension".equals(name) && isActive()) {
//            if(!value.equals(getAttributeValue("extension"))) {
//                if(new ExtensionChange().execute(this, value) && getParent() == null) {
//                    return getAttributeValue(name);
//                }
//            }
//        }
        return super.setAttributeValue(name, value);
    }
    
	public String getAsText() {
		return getAttributeValue("body");
	}

	public void edit(String body) {
		getModel().changeObjectAttribute(this, "body", body);
	}
	
	public void set(String name, String value) {
		if((!"NAME".equals(name) && !"EXTENSION".equals(name)) || !isActive() || !(getParent() instanceof FolderImpl)) {
			super.set(name, value);
		} else {
			rename0(value, name);
		}
	}
	
	public void fileRenamed(String name, String extension) {
		super.set("NAME", name);
		super.set("EXTENSION", extension);
		fireObjectChanged(null);
	}
	
	void rename0(String value, String attr) {
		FolderImpl p = (FolderImpl)getParent();
		IFile f =  p.getChildFile(toFileName(this));
		String n1 = get(attr);
		super.set(attr, value);
		String n2 = get(attr);
		IFile f2 =  p.getChildFile(toFileName(this));
		if(f2.exists() && !f.exists()) return;
		if(!n2.equals(n1)) {
			try {
				f.move(new Path(f.getParent().getFullPath() + "/" + toFileName(this)), true, null);
			} catch (Exception e) {
				super.set(attr, n1);
			}
		}			
	}
	
	public String getAbsolutePath() {
		if(!(getParent() instanceof FolderImpl)) return null;
		FolderImpl p = (FolderImpl)getParent();
		File f = p.getFile();
		if(f == null) return null;
		f = new File(f, toFileName(this));
		if(f.isFile()) {
			try {
				return f.getCanonicalPath().replace('\\', '/');
			} catch (Exception e) {
				//ignore - if file does not exist, do replacements in other way
			}
		}
		return f.getAbsolutePath().replace('\\', '/');
	}

	public IFile getFile() {
		if(!(getParent() instanceof FolderImpl)) return null;
		return ((FolderImpl)getParent()).getChildFile(toFileName(this));		
	}
	
	public Object getAdapter(Class adapter) {
		if(IResource.class == adapter || IFile.class == adapter) return getFile();
		return super.getAdapter(adapter);
	}
	
	private void saveUnopen() {
		if(!isModified() || !isActive()) return;
		XJob.addRunnableWithPriority(new XRunnable() {
			public String getId() {
				return "Save " + getPath();
			}
			public void run() {
				Display.getDefault().asyncExec(new Runnable(){
					public void run() {
						saveUnopen0();
					}
				});
			}
		});
	}

	private void saveUnopen0() {
		if(!isActive()) return;
		SpecialWizard w = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.objecteditor.SaveUnopenSpecialWizard");
		if(w == null) return;
		w.setObject(this);
		w.execute();
	}

	protected void onAttributeValueEdit(String name, String oldValue, String newValue) {
		if("body".equals(name) && listener != null) {
			listener.bodyChanged(newValue);
		}
	}
}
