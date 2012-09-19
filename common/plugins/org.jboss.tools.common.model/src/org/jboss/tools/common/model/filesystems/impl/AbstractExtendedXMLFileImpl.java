/*
 * Created on February 20, 2003, 4:02 PM
 */

package org.jboss.tools.common.model.filesystems.impl;

import java.io.StringReader;
import java.util.*;

import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.engines.impl.EnginesLoader;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.filesystems.BodySource;
import org.jboss.tools.common.model.filesystems.impl.AbstractXMLFileImpl;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.markers.ConstraintChecker;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.util.*;

/**
 *
 * @author  valera & glory
 */
public class AbstractExtendedXMLFileImpl extends AbstractXMLFileImpl {
    private static final long serialVersionUID = 7942041044569562286L;
	ConstraintChecker constraintChecker = new ConstraintChecker(this);
	
	ThreadSafeCopyFactory threadSafeCopyFactory = null;

    public AbstractExtendedXMLFileImpl() {}

    /**
     * Returns ready to be loaded copy of this object if and only if this object is being loaded by another thread.
     * Otherwise, returns null.
     * 
     * @return
     */
    XModelObject getThreadSafeCopy() {
    	return threadSafeCopyFactory == null ? null : threadSafeCopyFactory.getThreadSafeCopy();
    }

    public boolean hasChildren() {
        return true;
    }

    private static String ns = ".NAME.EXTENSION.overlapped.expanded._body_.actualBodyTimeStamp.correctBody.forceLoad."; //$NON-NLS-1$

    public String get(String name) {
        if(name.equals("_hasErrors_")) { //$NON-NLS-1$
            return super.get(ATTR_NAME_IS_INCORRECT);
        }
        if (getParent() != null && ns.indexOf("." + name + ".") < 0) { //$NON-NLS-1$ //$NON-NLS-2$
       		if(loadAttributeSeparately(name)) return super.get(name);
            loadChildren();
            
            XModelObject copy = getThreadSafeCopy();
            if(copy != null) {
            	return copy.get(name);
            }
            
        }
        return super.get(name);
    }
    
    /**
     * Used to avoid file loading when only one attribute is required.
     * This must be used with caution. Now only uri attribute in tld 
     * is processed as most critical.
     * @param xmlname
     * @return
     */    
    protected boolean shouldLoadAttributeSeparately(String xmlname) {
    	return false;
    }
    
    /**
     * @see shouldLoadAttributeSeparately.
     * @param xmlname
     * @return
     */    
    private boolean loadAttributeSeparately(String xmlname) {
        BodySource source = getBodySource();
        if(source == null) return threadSafeCopyFactory == null;

    	if(!shouldLoadAttributeSeparately(xmlname)) return false;
    	if(xmlname == null || xmlname.length() == 0) return false;
    	String oldvalue = super.get(xmlname);
    	if(oldvalue != null && oldvalue.length() > 0) return true;
        String body = source.get();
        Document doc = XMLUtil.getDocument(new StringReader(body));
        if(doc == null) return false;
        Element element = doc.getDocumentElement();
        if(element == null) return false;
        XModelObjectLoaderUtil util = new XModelObjectLoaderUtil();
        util.setSaveEntity(false);
    	String value = util.getAttribute(element, xmlname);
    	if(value == null || value.length() == 0) return false; 
    	super.set(xmlname, value);
    	return true;
    }
    
    public boolean isObjectEditable() {
    	return super.isObjectEditable() && (!XModelObjectConstants.YES.equals(get("_hasErrors_"))); //$NON-NLS-1$
    }

    public XModelObject[] getChildren() {
    	XModelObject copy = getThreadSafeCopy();
    	return (copy != null) ? copy.getChildren() : super.getChildren();
    }
    
    protected void loadChildren() {
    	getThreadSafeCopy();   		
 	
        BodySource source = getBodySource();
        if (source == null) return;
        
        threadSafeCopyFactory = new ThreadSafeCopyFactory(this);

        super.setBodySource(null);
        XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(this);
        String body = source.get();
        XModelObjectLoaderUtil.setTempBody(this, body);
        loader.load(this);

        if(threadSafeCopyFactory != null) {
        	threadSafeCopyFactory.destroy();
        	threadSafeCopyFactory = null;
        }

        if(!isIncorrect() && !isOverlapped()) {
        	runCheckerOnLoad();
        }
    }
    
    static class CheckerJob extends WorkspaceJob {
		List<AbstractExtendedXMLFileImpl> files = new ArrayList<AbstractExtendedXMLFileImpl>();

		public CheckerJob() {
			super("Checking constraints on load..."); //$NON-NLS-1$
		}
    	
		public void add(AbstractExtendedXMLFileImpl file) {
			synchronized (files) {
				files.add(file);
			}
			schedule();
		}

    	@Override
		public IStatus runInWorkspace(IProgressMonitor monitor)
				throws CoreException {
			while(true) {
    			AbstractExtendedXMLFileImpl file = null;
    			synchronized (files) {
    				if(files.isEmpty()) break;
    				file = files.remove(0);
    			}
    			file.getResourceMarkers().clear();
    			file.constraintChecker.check();
    		}
			
			return Status.OK_STATUS;
		}
    	
    }
    static CheckerJob checkerOnLoad = new CheckerJob();
   
    void runCheckerOnLoad() {
    	if(!isActive()) return;
    	XModelObject s = getParent();
    	while(s != null && s.getFileType() != XModelObject.SYSTEM) s = s.getParent();
    	if(s == null || !s.getModelEntity().getName().equals(XModelObjectConstants.ENT_FILE_SYSTEM_FOLDER)) {
    		return;
    	}
    	/*Runnable r = new Runnable() {
    		public void run() {
    	    	getResourceMarkers().clear();
    	   		constraintChecker.check();
    		}
    	};
    	Display.getDefault().asyncExec(r);*/
    	checkerOnLoad.add(this);
    }

    public void set(String name, String value) {
        super.set(name, value);
        if(ATTR_NAME_INCORRECT_BODY.equals(name) && value.length() > 0) { //$NON-NLS-1$
            set(ATTR_NAME_IS_INCORRECT, YES);
//            setErrors(value, hasDTD(), !hasDTD()); //never validate dtd
            int resolution = EntityXMLRegistration.getInstance().resolve(getModelEntity());
            if(EntityXMLRegistration.isSystemId(value)) resolution = EntityXMLRegistration.UNRESOLVED;
            setErrors(value, resolution == EntityXMLRegistration.DTD, resolution == EntityXMLRegistration.SCHEMA);
        }
    }

	public String getAsText() {
		return get(XModelObjectConstants.ATTR_NAME_BODY);
	}

	public void edit(String body) throws XModelException {
		edit(body, false);
	}
	
	protected boolean isForceLoadOn() {
		return XModelObjectConstants.TRUE.equals(get("forceLoad")); //$NON-NLS-1$
	}

    public void edit(String body, boolean update) throws XModelException {
        if(body == null) return;
        if(!isForceLoadOn() && body.equals(getAsText())) return;

		String entity = getModel().getEntityRecognizer().getEntityName(new EntityRecognizerContext(toFileName(this), getAttributeValue(XModelObjectConstants.ATTR_NAME_EXTENSION), body));
		if(entity == null || !entity.equals(getModelEntity().getName())) {
			String[] errors = (body.length() == 0) ? null : XMLUtil.getXMLErrors(new java.io.StringReader(body), false);
			if(errors == null || errors.length == 0) errors = new String[]{"Doctype has been changed. Please save file for the change to take effect in object model.    :0:0"}; //$NON-NLS-1$
			setErrors(body, errors);
			XModelImpl m = (XModelImpl)getModel();
			m.fireStructureChanged(this);
			if(!isOverlapped())	getResourceMarkers().update();
			return;			
		}

		boolean errors1 = (XModelObjectConstants.YES.equals(get("_hasErrors_"))); //$NON-NLS-1$
		AbstractExtendedXMLFileImpl f = getUpdatedFile(body, true);
        if(f == null) return;
        f.getChildren();
        
        XModelObjectImpl p = (XModelObjectImpl)getParent();
        XModelImpl m = (XModelImpl)getModel();
        boolean isOverlapped = isOverlapped();
		if(!isOverlapped)	getResourceMarkers().clear();		
		if(f.isIncorrect()) {
			getChildren();
			boolean fire = this.loaderError == null && f.loaderError != null;
			this.loaderError = f.loaderError;
			super.set(ATTR_NAME_INCORRECT_BODY, f.get(ATTR_NAME_INCORRECT_BODY));
			super.set(ATTR_NAME_IS_INCORRECT, YES);
			if(f.get("errors") != null) super.set("errors", f.get("errors")); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

			if(fire) changeTimeStamp();
			if(f.get("actualBodyTimeStamp") != null && !"-1".equals(f.get("actualBodyTimeStamp"))) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
				mergeAll(f, update);
			}
			m.fireStructureChanged(this);
			if(!isOverlapped) getResourceMarkers().update();
		} else if(isMergingChanges()) {
			String oldBody = get(ATTR_NAME_CORRECT_BODY);
			boolean changed = body != null && !body.equals(oldBody);
			set(ATTR_NAME_CORRECT_BODY, body);
			set("actualBodyTimeStamp", "0"); //$NON-NLS-1$ //$NON-NLS-2$
			long ots = getTimeStamp();
			mergeAll(f, update);
			if(changed && ots == getTimeStamp()) {
				changeTimeStamp();
			}
			set("actualBodyTimeStamp", "" + getTimeStamp()); //$NON-NLS-1$ //$NON-NLS-2$
			if(errors1) m.fireStructureChanged(this);
        	if(!isOverlapped) {
        		check();
        	}
		} else {
			//old edit by replace		
            p.removeChild_0(this);
            p.addChild_0(f);
            m.fireStructureChanged(p);
            f.setModified(true);
		    if(!isOverlapped) f.getResourceMarkers().update();
        }
    }
   
    public void check() {
    	constraintChecker.check();
    }
    
    protected void safeChangeTimeStamp() {
    	boolean b = ("" + getTimeStamp()).equals(get("actualBodyTimeStamp")); //$NON-NLS-1$ //$NON-NLS-2$
    	changeTimeStamp();
    	if(b) set("actualBodyTimeStamp", "" + getTimeStamp()); //$NON-NLS-1$ //$NON-NLS-2$
    }
    protected void mergeAll(XModelObject f, boolean update) throws XModelException {
   		merge(f, !update);
    }
    
    protected boolean isMergingChanges() {
    	return true;
    }
    
    protected String getProcessEntity() {
    	return null;
    }
    
	protected boolean hasDTD() {
		return true;
	}

    private AbstractExtendedXMLFileImpl getUpdatedFile(String body, boolean fire) {
        boolean errors1 = (XModelObjectConstants.YES.equals(get("_hasErrors_"))); //$NON-NLS-1$
        loaderError = null;
//      setErrors(body, hasDTD(), !hasDTD()); //never validate dtd
        int resolution = EntityXMLRegistration.getInstance().resolve(getModelEntity());
        if(EntityXMLRegistration.isSystemId(body)) resolution = EntityXMLRegistration.UNRESOLVED;
        setErrors(body, resolution == EntityXMLRegistration.DTD, resolution == EntityXMLRegistration.SCHEMA);
        boolean errors2 = (get("errors") != null && get("errors").length() > 0); //$NON-NLS-1$ //$NON-NLS-2$
        if(errors1 && errors2) {
            super.set(ATTR_NAME_INCORRECT_BODY, body); //$NON-NLS-1$
            if(fire) {
            	changeTimeStamp();
                if(isActive()) ((XModelImpl)getModel()).fireNodeChanged(this, getPath());
                setModified(true);
            }
        }
        AbstractExtendedXMLFileImpl f = (AbstractExtendedXMLFileImpl)getModel().createModelObject(getModelEntity().getName(), null);
        f.setAttributeValue(XModelObjectConstants.ATTR_NAME, getAttributeValue(XModelObjectConstants.ATTR_NAME));
        f.setAttributeValue(XModelObjectConstants.ATTR_NAME_EXTENSION, getAttributeValue(XModelObjectConstants.ATTR_NAME_EXTENSION));
        f.setBodySource(new SFBodySource(body));
        if(errors2) {
            f.set(ATTR_NAME_INCORRECT_BODY, body); //$NON-NLS-1$
            f.set("errors", super.get("errors")); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return f;
    }
    
    protected final void merge(XModelObject update, boolean fire) throws XModelException {
    	if(fire) {
    		fireObjectChanged(XModelTreeEvent.BEFORE_MERGE);
    	}
    	if(!YES.equals(update.get(ATTR_NAME_IS_INCORRECT))) {
    		super.set(ATTR_NAME_INCORRECT_BODY, ""); //$NON-NLS-1$ //$NON-NLS-2$
			super.set(XModelObjectConstants.ATTR_NAME_IS_INCORRECT,XModelObjectConstants.NO);
			super.set("errors", ""); //$NON-NLS-1$ //$NON-NLS-2$
			loaderError = null;
    	}
    	if(update instanceof AbstractExtendedXMLFileImpl) {
    		loaderError = ((AbstractExtendedXMLFileImpl)update).loaderError;
    	}
		Map<String,XModelObject> map = getChildrenForSaveAsMap();
        Set<String> set = null;
		XModelObject[] cs = update.getChildren();
		String entity = getProcessEntity();
		for (int i = 0; i < cs.length; i++) {
			if(entity != null && entity.equals(cs[i].getModelEntity().getName())) continue;
			XModelObject c = getChildByPath(cs[i].getPathPart());
            if(c == null) {
            	if(set == null) set = EnginesLoader.getChildrenToRemove(map, update);
            	c = EnginesLoader.findAppropriateChild(set, cs[i], map);
            	if(c == null) {
            		c = cs[i].copy();
            		addChild(c);
            	} else {
            		boolean has_id = c.getModelEntity().getAttribute(XModelObjectLoaderUtil.ATTR_ID_NAME) != null;
            		if(has_id) {
            			c.removeFromParent();
            			EnginesLoader.merge(c, cs[i], false);
            			addChild(c);
            		} else {
            			EnginesLoader.merge(c, cs[i], fire);
            		}
            	}
            } else if(c.getModelEntity().getName().equals(cs[i].getModelEntity().getName())) {
            	EnginesLoader.merge(c, cs[i], fire);
            } else {
            	removeChild(c);
            	addChild(cs[i].copy());
            }
			map.remove(c.getPathPart()); 
		}
		Iterator<XModelObject> it = map.values().iterator();
		while(it.hasNext()) {
			((XModelObject)it.next()).removeFromParent();
		}
        boolean doFire = false;
		for (int i = 0; i < cs.length; i++) {
			XModelObject c = getChildByPath(cs[i].getPathPart());
			if(c == null) continue;
			int ci = getIndexOfChild(c);
			if(ci == i) continue;
			doFire = true;
			move(ci, i, false);
		}
		if(fire && doFire) ((XModelImpl)getModel()).fireStructureChanged(this);
		mergeAttributes(update, fire);
    	if(fire) {
    		fireObjectChanged(XModelTreeEvent.AFTER_MERGE);
    	}
    }
     
	static String NO_MERGE_ATTRIBUTES = ".name.extension._lateload.isIncorrect.incorrectBody.expand."; //$NON-NLS-1$
    void mergeAttributes(XModelObject update, boolean fire) throws XModelException {
		XAttribute[] as = update.getModelEntity().getAttributes();
		for (int i = 0; i < as.length; i++) {
			String n = as[i].getName();
			if(NO_MERGE_ATTRIBUTES.indexOf("." + n + ".") >= 0) continue; //$NON-NLS-1$ //$NON-NLS-2$
			XModelObjectLoaderUtil.mergeAttributeComment(this, update, as[i], fire);
			String ov = getAttributeValue(n);
			String nv = update.getAttributeValue(n);
			if(ov == null || ov.equals(nv)) continue;
			if(fire) {
				getModel().changeObjectAttribute(this, n, nv);
			} else {
				setAttributeValue(n, nv);
			}
		}
		XModelObjectLoaderUtil.mergeFinalComment(this, update, fire);
    }
    
    private Map<String,XModelObject> getChildrenForSaveAsMap() {
    	String entity = getProcessEntity();
    	XModelObject[] cs = getChildrenForSave();
    	Map<String,XModelObject> map = new HashMap<String,XModelObject>();
    	for (int i = 0; i < cs.length; i++) { 
			if(entity != null && entity.equals(cs[i].getModelEntity().getName())) continue;
    		map.put(cs[i].getPathPart(), cs[i]);
    	} 
    	return map;
    }

}

class SFBodySource implements BodySource {
    String body;

    public SFBodySource(String body) {
        this.body = body;
    }

    public String get() {
        return body;
    }

    public boolean write(Object object) {
        return true;
    }
}
