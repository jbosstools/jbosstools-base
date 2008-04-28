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
package org.jboss.tools.common.model.ui.preferences;

import java.util.List;
import org.eclipse.ui.IWorkbench;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Composite;
import org.jboss.tools.common.model.ui.attribute.XAttributeSupport;
import org.jboss.tools.common.model.ui.attribute.adapter.IModelPropertyEditorAdapter;
import org.jboss.tools.common.model.ui.util.ModelUtilities;
import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.ui.IWorkbenchPreferencePage;

import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.model.XModel;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.options.impl.SharableElementImpl;

public class XMOBasedPreferencesPage extends PreferencePage implements IPreferencePageExt, IWorkbenchPreferencePage{
	
  public Composite control;
  protected XModel preferenceModel = ModelUtilities.getPreferenceModel();
  protected XAttributeSupport support;
  protected XModelObject object;
  
  String path;
  
  public XMOBasedPreferencesPage(XModelObject xmo) {
  	this(xmo, null);
  }
  
  public XMOBasedPreferencesPage(XModelObject xmo, XEntityData data) {
  	object = xmo;
	path = xmo.getPath();
	support = new XAttributeSupport();
	if(data == null) support.init(xmo); else support.init(xmo, data, true);
	support.setAutoStore(false);
  }
  
	public void dispose() {
		super.dispose();
		if (support!=null) support.dispose();
		support = null;
	}

  public String getTitle() {
  	return support.getTitle();
  }

	public void init(IWorkbench workbench)  {
	}

	public Control createContents(Composite parent)  {
		control = support.createControl(parent); 
		return control;
    }
  
	public boolean isValid() {
		return super.isValid();
	}

	public boolean performCancel() {
		support.load();
		return super.performCancel();
	}

	public void performDefaults() {
		if(object instanceof SharableElementImpl) {
			SharableElementImpl s = (SharableElementImpl)object;
			List l = support.getAdapterList();
			for (int i = 0; i < l.size(); i++) {
				IModelPropertyEditorAdapter a = (IModelPropertyEditorAdapter)l.get(i);
				String n = a.getAttribute().getName();
				String v = s.getDefaultValue(n);
				if(v != null && v.length() > 0) a.setValue(v);
			}
		}
		super.performDefaults();
	}

	/**
	 * @see org.eclipse.jface.dialogs.IDialogPage#performHelp()
	 */
	public void performHelp() {
	}

	/**
	 * @see org.eclipse.jface.preference.IPreferencePage#performOk()
	 */
  	public boolean performOk() {
		support.store();
		support.save();    
		return true;
	}
	
	public void initPageProperties() {
		
	}
	
	public final XAttributeSupport getAttributeSupport() {
		return support;
	}
	
	public final XModelObject getObject() {
		return object;
	}
	
	public final XAttributeSupport getSupport() {
		return support;
	}
	
	public final Composite getSupportControl() {
		return control;
	}
	
}
