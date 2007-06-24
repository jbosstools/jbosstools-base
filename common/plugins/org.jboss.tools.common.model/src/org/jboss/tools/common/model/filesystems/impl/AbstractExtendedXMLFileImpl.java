/*
 * Created on February 20, 2003, 4:02 PM
 */

package org.jboss.tools.common.model.filesystems.impl;

import java.io.StringReader;
import java.util.*;

import org.eclipse.swt.widgets.Display;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.engines.impl.EnginesLoader;
import org.jboss.tools.common.model.filesystems.BodySource;
import org.jboss.tools.common.model.filesystems.impl.AbstractXMLFileImpl;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.markers.ConstraintChecker;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.util.*;

/**
 *
 * @author  valera & glory
 */
public class AbstractExtendedXMLFileImpl extends AbstractXMLFileImpl {
    private static final long serialVersionUID = 7942041044569562286L;
	ConstraintChecker constraintChecker = new ConstraintChecker(this);

    public AbstractExtendedXMLFileImpl() {}

    public boolean hasChildren() {
        return true;
    }

    private static String ns = ".NAME.EXTENSION.overlapped.expanded._body_.actualBodyTimeStamp.correctBody.forceLoad.";

    public String get(String name) {
        if(name.equals("_hasErrors_")) {
            return super.get("isIncorrect");
        }
        if (getParent() != null && ns.indexOf("." + name + ".") < 0) {
       		if(loadAttributeSeparately(name)) return super.get(name);
            loadChildren();
            
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
        if(source == null) return true;
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
    	return super.isObjectEditable() && (!"yes".equals(get("_hasErrors_")));
    }
    
    protected void loadChildren() {
        BodySource source = getBodySource();
        if (source == null) return;
        super.setBodySource(null);
        XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(this);
        String body = source.get();
        XModelObjectLoaderUtil.setTempBody(this, body);
        loader.load(this);
        if(!isIncorrect() && !isOverlapped()) {
        	runCheckerOnLoad();
        }
    }
    
    void runCheckerOnLoad() {
    	if(!isActive()) return;
    	XModelObject s = getParent();
    	while(s != null && s.getFileType() != XModelObject.SYSTEM) s = s.getParent();
    	if(s == null || !s.getModelEntity().getName().equals("FileSystemFolder")) {
    		return;
    	}
    	Runnable r = new Runnable() {
    		public void run() {
    	    	getResourceMarkers().clear();
    	    	try {
    	    		constraintChecker.check();
    	    	} catch (Exception e) {
    	    		if(ModelPlugin.isDebugEnabled()) {
    	    			ModelPlugin.log(e);
    	    		}
    	    	}    	
    		}
    	};
    	Display.getDefault().asyncExec(r);
    }

    public void set(String name, String value) {
        super.set(name, value);
        if("incorrectBody".equals(name) && value.length() > 0) {
            set("isIncorrect", "yes");
            setErrors(value, hasDTD(), !hasDTD()); //schema!
        }
    }

	public String getAsText() {
		return get("body");
	}

	public void edit(String body) {
		edit(body, false);
	}
	
	protected boolean isForceLoadOn() {
		return "true".equals(get("forceLoad"));
	}

    public void edit(String body, boolean update) {
        if(body == null) return;
        if(!isForceLoadOn() && body.equals(getAsText())) return;

		String entity = getModel().getEntityRecognizer().getEntityName(getAttributeValue("extension"), body);
		if(!entity.equals(getModelEntity().getName())) {
			String[] errors = (body.length() == 0) ? null : XMLUtil.getXMLErrors(new java.io.StringReader(body), false);
			if(errors == null || errors.length == 0) errors = new String[]{"Doctype has been changed. Please save file for the change to take effect in object model.    :0:0"};
			setErrors(body, errors);
			XModelImpl m = (XModelImpl)getModel();
			m.fireStructureChanged(this);
			if(!isOverlapped())	getResourceMarkers().update();
			return;			
		}

		boolean errors1 = ("yes".equals(get("_hasErrors_")));
		AbstractExtendedXMLFileImpl f = getUpdatedFile(body, true);
        if(f == null) return;
        
        XModelObjectImpl p = (XModelObjectImpl)getParent();
        XModelImpl m = (XModelImpl)getModel();
        boolean isOverlapped = isOverlapped();
		if(!isOverlapped)	getResourceMarkers().clear();		
		if(f.isIncorrect()) {
			getChildren();
			super.set("incorrectBody", f.get("incorrectBody"));
			super.set("isIncorrect", "yes");
			if(f.get("errors") != null) super.set("errors", f.get("errors"));

			f.getChildren();
			if(f.get("actualBodyTimeStamp") != null && !"-1".equals(f.get("actualBodyTimeStamp"))) {
				mergeAll(f, update);
			}
			m.fireStructureChanged(this);
			if(!isOverlapped) getResourceMarkers().update();
		} else if(isMergingChanges()) {
			set("correctBody", body);
			set("actualBodyTimeStamp", "0");
			mergeAll(f, update);
			set("actualBodyTimeStamp", "" + getTimeStamp());
			if(errors1) m.fireStructureChanged(this);
        	try {
        		if(!isOverlapped) constraintChecker.check();
        	} catch (Exception e) {
        		ModelPlugin.log(e);
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
    
    protected void mergeAll(XModelObject f, boolean update) {
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
        boolean errors1 = ("yes".equals(get("_hasErrors_")));
        setErrors(body, hasDTD(), !hasDTD()); ///schema!
        boolean errors2 = (get("errors") != null && get("errors").length() > 0);
        if(errors1 && errors2) {
            super.set("incorrectBody".intern(), body);
            if(fire) {
            	changeTimeStamp();
                if(isActive()) ((XModelImpl)getModel()).fireNodeChanged(this, getPath());
                setModified(true);
            }
        }
        AbstractExtendedXMLFileImpl f = (AbstractExtendedXMLFileImpl)getModel().createModelObject(getModelEntity().getName(), null);
        f.setAttributeValue("name", getAttributeValue("name"));
        f.setAttributeValue("extension", getAttributeValue("extension"));
        if(errors2) {
            f.set("incorrectBody", body);
            f.set("errors", super.get("errors"));
        }
        f.setBodySource(new SFBodySource(body));
        return f;
    }
    
    protected final void merge(XModelObject update, boolean fire) {
    	if(!"yes".equals(update.get("isIncorrect"))) {
    		super.set("incorrectBody", "");
			super.set("isIncorrect","no");
			super.set("errors", "");
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
            		EnginesLoader.merge(c, cs[i], fire);
            	}
            } else if(c.getModelEntity().getName().equals(cs[i].getModelEntity().getName())) {
            	EnginesLoader.merge(c, cs[i], fire);
            } else {
            	removeChild(c);
            	addChild(cs[i].copy());
            }
			map.remove(c.getPathPart()); 
		}
		Iterator it = map.values().iterator();
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
    }
     
	static String NO_MERGE_ATTRIBUTES = ".name.extension._lateload.isIncorrect.incorrectBody.expand.";
    void mergeAttributes(XModelObject update, boolean fire) {
		XAttribute[] as = update.getModelEntity().getAttributes();
		for (int i = 0; i < as.length; i++) {
			String n = as[i].getName();
			if(NO_MERGE_ATTRIBUTES.indexOf("." + n + ".") >= 0) continue;
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
