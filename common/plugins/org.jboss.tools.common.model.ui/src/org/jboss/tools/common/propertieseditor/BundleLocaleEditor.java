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
package org.jboss.tools.common.propertieseditor;

import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.propertieseditor.bundlemodel.*;
import org.jboss.tools.common.model.ui.action.CommandBar;
import org.eclipse.swt.graphics.Color;

public class BundleLocaleEditor extends PropertiesEditor {
	BundleModel bundleModel;
	
	public void dispose() {
		super.dispose();
		if (bundleModel!=null) bundleModel.dispose();
		bundleModel = null;
	}

	public void setBundleModel(BundleModel bundleModel) {
		this.bundleModel = bundleModel;
	}
	
	protected Color getItemColor(int i) {
		if(bundleModel == null) return DEFAULT_COLOR;
		XModelObject o = helper.getModelObject(i);
		PropertyModel pm = bundleModel.getPropertyModel(o, bundleModel.getCurrentLocale());
		if(pm == null) return GREYED_COLOR;
		return(pm.hasValue(bundleModel.getCurrentLocale())) ? DEFAULT_COLOR : GREYED_COLOR;
	}

	protected void edit() {
		XModelObject o = helper.getModelObject(xtable.getSelectionIndex());
		if(o != null) {
			callAction(o, "Properties.Edit");
			updatePropertyModel();
		} 
	}
	
	protected void add() {
		long ts = helper.getModelObject().getTimeStamp();
		super.add();
		updatePropertyModel();
		if(ts != helper.getModelObject().getTimeStamp()) bundleModel.setModified(true);
	}
	
	protected void delete() {
		long ts = helper.getModelObject().getTimeStamp();
		super.delete();
		if(ts != helper.getModelObject().getTimeStamp()) bundleModel.setModified(true);
	}
	
	private void updatePropertyModel() {
		XModelObject o = helper.getModelObject(xtable.getSelectionIndex());
		if(o == null) return;
		PropertyModel pm = bundleModel.getPropertyModel(o, bundleModel.getCurrentLocale());
		if(pm != null) pm.commit();
	}
	
	protected boolean isReadOnly() {
		return (bundleModel == null || !bundleModel.isEditable());
	}
	
	protected void setMargins(CommandBar bar) {
		bar.getLayout().setMargins(10,10,0,5);
	}


}
