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

import org.w3c.dom.*;

import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.action.impl.XEntityDataImpl;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.filesystems.impl.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.xml.SAXValidator;

public class SimpleWebFileLoader implements SerializingLoader {
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
        	return;
        }
        Element element = doc.getDocumentElement();

        //String namespace = 
        loadNamespace(element, object);
////        String postfix = (namespace == null) ? "" : ":" + namespace;
        String postfix = "";
        element.setAttribute("NAME" + postfix, object.getAttributeValue("name"));
        element.setAttribute("EXTENSION" + postfix, object.getAttributeValue("extension"));

        util.load(element, object);

        setEncoding(object, body);
		loadPublicId(object, doc);

		object.set("actualBodyTimeStamp", "" + object.getTimeStamp());
    }
    
    protected Document loadDocument(XModelObject object, String body) {
        String[] errors = XMLUtil.getXMLErrors(new StringReader(body), isCheckingDTD());
        if(isCheckingSchema()) {
       		errors = new SAXValidator().getXMLErrors(new StringReader(body));
        }
        if(errors != null && errors.length > 0) {
            object.setAttributeValue("isIncorrect", "yes");
            object.set("correctBody", "");
            object.setAttributeValue("incorrectBody", body);
			object.set("actualBodyTimeStamp", "-1");
//            return;
        } else {
            object.setAttributeValue("isIncorrect", "no");
			object.set("correctBody", body);
			object.set("actualBodyTimeStamp", "0");
            object.setAttributeValue("incorrectBody", "");
        }
        return XMLUtil.getDocument(new StringReader(body));
    }
    
    protected String loadNamespace(Element element, XModelObject object) {
        String rootName = element.getNodeName();
        String namespace = null;
        if(rootName.indexOf(':') > 0) namespace = rootName.substring(0, rootName.indexOf(':'));
        if(namespace != null) {
        	util.setNamespace(namespace);
        	object.setAttributeValue("namespace", namespace);
        } else {
        	util.setNamespace(null);
        	object.setAttributeValue("namespace", "");
        }
        return namespace;
    }
    
    protected void loadPublicId(XModelObject object, Document doc) {
		XModelEntity entity = object.getModelEntity();
		if(entity.getAttribute("publicId") != null) {
			NodeList nl = doc.getChildNodes();
			for (int i = 0; i < nl.getLength(); i++) {
				Node n = nl.item(i);
				if(n instanceof DocumentType) {
					DocumentType dt = (DocumentType)n;
					String s = dt.getSystemId();
					if(s == null) s = "";
					object.setAttributeValue("systemId", s);
					s = dt.getPublicId();
					if(s == null) s = "";
					object.setAttributeValue("publicId", s);
				}
			}
		}
    }
    
	protected void setEncoding(XModelObject object, String body) {
		String encoding = XModelObjectLoaderUtil.getEncoding(body);
		if(encoding == null) encoding = "";
		object.setAttributeValue(XModelObjectConstants.ATTR_NAME_ENCODING, encoding);
	}
    
    public boolean update(XModelObject object) {
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
        if("yes".equals(object.get("isIncorrect"))) {
            XModelObjectLoaderUtil.setTempBody(object, object.get("incorrectBody"));
            return true;
        }
        String main = object.get("body");
        if(main == null) return false;
		XModelObjectLoaderUtil.setTempBody(object, main);
        return true;
    }

    public String serializeObject(XModelObject object) {
        String systemId = object.getAttributeValue("systemId");
        String publicId = object.getAttributeValue("publicId");
    	String rootName = getRootName(object);
        Element element = createRootElement(rootName, publicId, systemId);
        return serializeToElement(element, object);
    }
    
    protected String getRootName(XModelObject object) {
    	String namespace = object.getAttributeValue("namespace");
    	String rootName = object.getModelEntity().getXMLSubPath();
       	if(namespace != null && namespace.length() > 0) {
       		util.setNamespace(namespace);
       		rootName = namespace + ":" + rootName;
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
////        String postfix = (namespace == null) ? "" : ":" + namespace;
            element.removeAttribute("NAME");
            element.removeAttribute("EXTENSION");
            return serialize(element, object);
        } catch (Exception e) {
        	ModelPlugin.log(e);
            return null;
        }
    }
    
    protected String getDocName() {
    	return "taglib";///TLD_DOC_QUALIFIEDNAME
    }
    
    /**
     * Checks encoding
     * @param element
     * @param object
     * @return
     */
    
    public static String serialize(Element element, XModelObject object) throws Exception {
    	String encoding = object.getAttributeValue(XModelObjectConstants.ATTR_NAME_ENCODING);
		StringWriter sw = new StringWriter();
		try {
			XModelObjectLoaderUtil.serialize(element.getOwnerDocument(), sw, encoding);
		} catch (UnsupportedEncodingException uee) {
			if("UTF-8".equals(encoding)) return null;
			ServiceDialog d = object.getModel().getService();
			XEntityData data = XEntityDataImpl.create(new String[][]{
				{object.getModelEntity().getName(), "yes"},
				{XModelObjectConstants.ATTR_NAME_ENCODING, "no"}
			});
			data.setValue(XModelObjectConstants.ATTR_NAME_ENCODING, "UTF-8");
			String message = "Encoding " + encoding + " is not supported. Please enter correct value.";
			int q = d.showDialog("Error", message, new String[]{"OK"}, data, ServiceDialog.ERROR);
			encoding = (q != 0) ? "UTF-8" : data.getValue(XModelObjectConstants.ATTR_NAME_ENCODING);
			object.setAttributeValue(XModelObjectConstants.ATTR_NAME_ENCODING, encoding);
			return serialize(element, object);
		}
        return sw.toString();
    }

	public void loadFragment(XModelObject object, Element element) {
		util.load(element, object);		
	}

}
