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
package org.jboss.tools.common.model.options.impl;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.XModelImpl;
import org.jboss.tools.common.model.options.PreferenceModelUtilities;
import org.jboss.tools.common.model.undo.XUndoManager;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.*;

public class HiddenPaletteTabsHandler extends AbstractHandler {
    SpecialWizard wizard = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.views.palette.editor.HiddenPaletteTabsWizard");

    public HiddenPaletteTabsHandler() {}

    public boolean isEnabled(XModelObject object) {
        return (wizard != null && object != null && object.isObjectEditable());
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        wizard = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.ui.views.palette.editor.HiddenPaletteTabsWizard");
        Map<String,XModelObject> objects = new HashMap<String,XModelObject>();
        List<String[]> dataList = new ArrayList<String[]>();
        collect(object, "", objects, dataList);
        
        String[][] vs = dataList.toArray(new String[0][]);
        if(p == null) p = new Properties();
        p.put("data", vs);
        p.setProperty("help", "SharablePalette_HiddenTabs");
        p.setProperty("expandingLevel", "1");
        wizard.setObject(p);
        if(wizard.execute() != 0) return;
        execute(object.getModel(), vs, objects);
    }
    
    private void collect(XModelObject object, String prefix, Map<String,XModelObject> objects, List<String[]> dataList) {
    	XModelObject[] cs = object.getChildren();
    	for (int i = 0; i < cs.length; i++) {
    		String path = prefix + "/" + cs[i].getAttributeValue("name");
    		String hidden = cs[i].getAttributeValue("hidden");
    		if(hidden == null) hidden = "no";
    		dataList.add(new String[]{path, hidden});
    		objects.put(path, cs[i]);
    		String kind = cs[i].getAttributeValue("element type");
    		if("group".equals(kind)) collect(cs[i], path, objects, dataList);
    	}
    }

	public void execute(XModel model, String[][] vs, Map objects) {
		fireTransactionEvent("transaction_begin");
		try {
			XUndoManager undo = model.getUndoManager();
			undo.beginTransaction();

	        for (int i = 0; i < vs.length; i++) {
	        	XModelObject o = (XModelObject)objects.get(vs[i][0]);
	            if(o != null) o.getModel().changeObjectAttribute(o, "hidden", vs[i][1]);
	        }

			undo.commitTransaction();
			model.saveOptions();
		} finally {
			fireTransactionEvent("transaction_end");
		}
	}
    
	private void fireTransactionEvent(String kind) {
		XModelImpl m = (XModelImpl)PreferenceModelUtilities.getPreferenceModel();
		m.fireStructureChanged(m.getByPath("%Palette%"), 2, kind);
	}
	
}
