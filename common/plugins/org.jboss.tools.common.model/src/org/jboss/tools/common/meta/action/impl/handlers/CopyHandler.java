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
package org.jboss.tools.common.meta.action.impl.handlers;

import java.util.*;
import org.eclipse.core.resources.*;
import org.eclipse.swt.dnd.*;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.part.ResourceTransfer;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.ModelFeatureFactory;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;

public class CopyHandler extends AbstractHandler {
	
	static Transfer modelTransfer;
	
	static {
		try {
			modelTransfer = (Transfer)ModelFeatureFactory.getInstance().createFeatureInstance("org.jboss.tools.common.model.ui.dnd.ModelTransfer");
		} catch (Exception e) {
			ModelPlugin.log("Model transfer class not found.");
		}
	}

    public CopyHandler() {}

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        object.getModel().getModelBuffer().clear();
        object.getModel().getModelBuffer().addSource(object);
        if(p == null || !"true".equals(p.getProperty("isDrag"))) {
        	setClipboard(object.getModel());
        }
    }

    public boolean getSignificantFlag(XModelObject object) {
        return false;
    }

    public boolean isEnabled(XModelObject object) {
        return (object != null);
    }

    public void executeHandler(XModelObject object, XModelObject[] objects, java.util.Properties p) throws Exception {
        if(!isEnabled(object, objects)) return;
        XModelBuffer buffer = object.getModel().getModelBuffer();
        buffer.clear();
        for (int i = 0; i < objects.length; i++) buffer.addSource(objects[i]);
        if(p == null || !"true".equals(p.getProperty("isDrag"))) {
        	setClipboard(object.getModel());
        }
    }

    public void setDefaultData(XModelObject object) {}
    
    void setClipboard(XModel model) {
    	XModelBuffer buffer = model.getModelBuffer();
    	
        List<IResource> resources = new ArrayList<IResource>();
        List<String> files = new ArrayList<String>();
        StringBuffer texts = new StringBuffer();
        List<String> paths = new ArrayList<String>();
        for (int i = 0; i < buffer.getSize(); i++) {
            XModelObject o = buffer.source(i);
            IResource resource = (IResource)o.getAdapter(IResource.class);
            if(resource != null && (resource instanceof IFile || resource instanceof IContainer)) {
            	resources.add(resource);
            }
            if(texts.length() > 0) texts.append(' ');
            texts.append(o.getPresentationString());
            if(resource instanceof IFile) {
            	files.add(resource.getLocation().toOSString());
            }
            paths.add("" + o.getPath());
       }

       Clipboard c = new Clipboard(Display.getCurrent());
       List<Object> dataList = new ArrayList<Object>();
       dataList.add(resources.toArray(new IResource[0]));
       if(files.size() > 0) dataList.add(files.toArray(new String[0]));
       dataList.add(texts.toString());
       dataList.add(paths.toArray(new String[0]));
       Object[] data = dataList.toArray(new Object[0]);
       Transfer[] t = (files.size() > 0) 
       	? new Transfer[]{ResourceTransfer.getInstance(), FileTransfer.getInstance(), TextTransfer.getInstance(), modelTransfer}
                   : new Transfer[]{ResourceTransfer.getInstance(), TextTransfer.getInstance(), modelTransfer};
                c.setContents(data, t);
    }

}

