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
package org.jboss.tools.common.model.loaders.impl;

import java.io.*;
import java.text.MessageFormat;

import org.w3c.dom.*;

import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.action.impl.XEntityDataImpl;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;

public class SimpleWebFileLoader implements SerializingLoader, XModelObjectConstants {
    protected XModelObjectLoaderUtil util = createUtil();

    public SimpleWebFileLoader() {
    	util.setSaveEntity(false);
    }
    
    public XModelObjectLoaderUtil getUtil() {
    	return util;
    }

    protected XModelObjectLoaderUtil createUtil() {
        return new XModelObjectLoaderUtil();
    }
    
    protected boolean isCheckingDTD() {
    	return true;
    }
    
    protected boolean isCheckingSchema() {
    	return !isCheckingDTD();
    }

    public void load(XModelObject object) {
         String body = XModelObjectLoaderUtil.getTempBody(object);
        Document doc = loadDocument(object, body);
        if(doc == null) {
        	XModelObjectLoaderUtil.addRequiredChildren(object);
        	return;
        }
        Element element = doc.getDocumentElement();

        //String namespace = 
        loadNamespace(element, object);
////        String postfix = (namespace == null) ? "" : ":" + namespace;
        String postfix = ""; //$NON-NLS-1$
        element.setAttribute(XML_ATTR_NAME + postfix, object.getAttributeValue(ATTR_NAME));
        element.setAttribute("EXTENSION" + postfix, object.getAttributeValue(ATTR_NAME_EXTENSION)); //$NON-NLS-1$

        util.load(element, object);
        String loadingError = util.getError();

        setEncoding(object, body);
		loadPublicId(object, doc);

		object.set("actualBodyTimeStamp", "" + object.getTimeStamp()); //$NON-NLS-1$ //$NON-NLS-2$
		
		((AbstractXMLFileImpl)object).setLoaderError(loadingError);
		if(!((AbstractXMLFileImpl)object).isIncorrect() && loadingError != null) {
			object.setAttributeValue(ATTR_NAME_IS_INCORRECT, YES);
			object.setAttributeValue(ATTR_NAME_INCORRECT_BODY, body);
			object.set("actualBodyTimeStamp", "" + object.getTimeStamp()); //$NON-NLS-1$ //$NON-NLS-2$
		}
    }
    
    protected Document loadDocument(XModelObject object, String body) {
        int resolution = EntityXMLRegistration.getInstance().resolve(object.getModelEntity());
        if(EntityXMLRegistration.isSystemId(body)) resolution = EntityXMLRegistration.UNRESOLVED;
        String[] errors = 
			XMLUtil.getXMLErrors(new StringReader(body), resolution == EntityXMLRegistration.DTD && isCheckingDTD(), resolution == EntityXMLRegistration.SCHEMA);
        if(errors != null && errors.length > 0) {
            object.setAttributeValue(ATTR_NAME_IS_INCORRECT, YES);
            object.set(ATTR_NAME_CORRECT_BODY, ""); //$NON-NLS-1$
            object.setAttributeValue(ATTR_NAME_INCORRECT_BODY, body);
			object.set("actualBodyTimeStamp", "-1"); //$NON-NLS-1$ //$NON-NLS-2$
//            return;
        } else {
            object.setAttributeValue(ATTR_NAME_IS_INCORRECT, NO);
			object.set(ATTR_NAME_CORRECT_BODY, body);
			object.set("actualBodyTimeStamp", "0"); //$NON-NLS-1$ //$NON-NLS-2$
            object.setAttributeValue(ATTR_NAME_INCORRECT_BODY, ""); //$NON-NLS-1$
        }
        return XMLUtil.getDocument(new StringReader(body));
    }
    
    protected String loadNamespace(Element element, XModelObject object) {
        String rootName = element.getNodeName();
        String namespace = null;
        if(rootName.indexOf(':') > 0) namespace = rootName.substring(0, rootName.indexOf(':'));
        if(namespace != null) {
        	util.setNamespace(namespace);
        	object.setAttributeValue("namespace", namespace); //$NON-NLS-1$
        } else {
        	util.setNamespace(null);
        	object.setAttributeValue("namespace", ""); //$NON-NLS-1$ //$NON-NLS-2$
        }
        return namespace;
    }
    
    protected void loadPublicId(XModelObject object, Document doc) {
		XModelEntity entity = object.getModelEntity();
		if(entity.getAttribute("publicId") != null) { //$NON-NLS-1$
			NodeList nl = doc.getChildNodes();
			for (int i = 0; i < nl.getLength(); i++) {
				Node n = nl.item(i);
				if(n instanceof DocumentType) {
					DocumentType dt = (DocumentType)n;
					String s = dt.getSystemId();
					if(s == null) s = ""; //$NON-NLS-1$
					object.setAttributeValue("systemId", s); //$NON-NLS-1$
					s = dt.getPublicId();
					if(s == null) s = ""; //$NON-NLS-1$
					object.setAttributeValue("publicId", s); //$NON-NLS-1$
				}
			}
		}
    }
    
	protected void setEncoding(XModelObject object, String body) {
		String encoding = XModelObjectLoaderUtil.getEncoding(body);
		if(encoding == null) encoding = ""; //$NON-NLS-1$
		object.setAttributeValue(XModelObjectConstants.ATTR_NAME_ENCODING, encoding);
	}
    
    public boolean update(XModelObject object) throws XModelException {
		XModelObject p = object.getParent();
		if (p == null) return true;
		FolderLoader fl = (FolderLoader)p;

		String body = fl.getBodySource(FileAnyImpl.toFileName(object)).get();
		FileAnyImpl f = ((FileAnyImpl)object);
		f.setUpdateLock();
		try {
			f.edit(body);
		} finally {
			f.releaseUpdateLock();
		}
		object.setModified(false);
		XModelObjectLoaderUtil.updateModifiedOnSave(object);
		return true;
    }

    public boolean save(XModelObject object) {
        if (!object.isModified()) return true;
        if(YES.equals(object.get(ATTR_NAME_IS_INCORRECT))) {
            XModelObjectLoaderUtil.setTempBody(object, object.get(ATTR_NAME_INCORRECT_BODY));
            return true;
        }
        String main = object.get(ATTR_NAME_BODY);
        if(main == null) return false;
		XModelObjectLoaderUtil.setTempBody(object, main);
        return true;
    }

    public String serializeObject(XModelObject object) {
        Element element = createRootElement(object);
        return serializeToElement(element, object);
    }
   
    public Element createRootElement(XModelObject object) {
        String systemId = object.getAttributeValue("systemId"); //$NON-NLS-1$
        String publicId = object.getAttributeValue("publicId"); //$NON-NLS-1$
    	String rootName = getRootName(object);
        return createRootElement(rootName, publicId, systemId);
    }
    
    protected String getRootName(XModelObject object) {
    	String namespace = object.getAttributeValue("namespace"); //$NON-NLS-1$
    	String rootName = object.getModelEntity().getXMLSubPath();
       	if(namespace != null && namespace.length() > 0) {
       		util.setNamespace(namespace);
       		rootName = namespace + ":" + rootName; //$NON-NLS-1$
       	} else {
       		util.setNamespace(null);
       	}
       	return rootName;
    }
    
    protected Element createRootElement(String rootName, String publicId, String systemId) {
        return (systemId == null || publicId == null)
    		? XMLUtil.createDocumentElement(rootName) : (publicId.length() == 0)
    		? XMLUtil.createDocumentElement(rootName, getDocName(), null, systemId, null)
    		: XMLUtil.createDocumentElement(rootName, getDocName(), publicId, systemId, null);
    }
    
    protected String serializeToElement(Element element, XModelObject object) {
        try {
            util.saveAttributes(element, object);
            util.saveChildren(element, object);
            util.saveFinalComment(element, object);
////        String postfix = (namespace == null) ? "" : ":" + namespace;
            element.removeAttribute(XModelObjectConstants.XML_ATTR_NAME);
            element.removeAttribute("EXTENSION"); //$NON-NLS-1$
            return serialize(element, object);
        } catch (IOException e) {
        	ModelPlugin.getPluginLog().logError(e);
        } catch (XModelException e) {
        	ModelPlugin.getPluginLog().logError(e);
        }
        return null;
    }
    
    protected String getDocName() {
    	return "taglib";///TLD_DOC_QUALIFIEDNAME //$NON-NLS-1$
    }
    
    /**
     * Checks encoding
     * @param element
     * @param object
     * @return
     */
    
    public static String serialize(Element element, XModelObject object) throws XModelException, IOException {
    	String encoding = object.getAttributeValue(XModelObjectConstants.ATTR_NAME_ENCODING);
		StringWriter sw = new StringWriter();
		try {
			XModelObjectLoaderUtil.serialize(element.getOwnerDocument(), sw, encoding);
		} catch (UnsupportedEncodingException uee) {
			if("UTF-8".equals(encoding)) return null; //$NON-NLS-1$
			ServiceDialog d = object.getModel().getService();
			XEntityData data = XEntityDataImpl.create(new String[][]{
				{object.getModelEntity().getName(), XModelObjectConstants.YES},
				{XModelObjectConstants.ATTR_NAME_ENCODING, XModelObjectConstants.NO}
			});
			data.setValue(XModelObjectConstants.ATTR_NAME_ENCODING, "UTF-8"); //$NON-NLS-1$
			String message = MessageFormat
					.format(
							"Encoding {0} is not supported. Please enter correct value.",
							encoding);
			int q = d.showDialog("Error", message, new String[]{"OK"}, data, ServiceDialog.ERROR);
			encoding = (q != 0) ? "UTF-8" : data.getValue(XModelObjectConstants.ATTR_NAME_ENCODING); //$NON-NLS-1$
			object.setAttributeValue(XModelObjectConstants.ATTR_NAME_ENCODING, encoding);
			return serialize(element, object);
		}
        return sw.toString();
    }

	public void loadFragment(XModelObject object, Element element) {
		util.load(element, object);		
	}

}
