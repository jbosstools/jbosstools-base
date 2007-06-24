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
package org.jboss.tools.common.editor.form;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;

import org.jboss.tools.common.model.ui.attribute.XAttributeSupport;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XEntityData;
import org.jboss.tools.common.meta.action.impl.XEntityDataImpl;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.ui.forms.ExpandableForm;
import org.jboss.tools.common.model.ui.widgets.DefaultSettings;
import org.jboss.tools.common.model.ui.widgets.IWidgetSettings;
import org.jboss.tools.common.model.ui.widgets.WhiteSettings;

public class SamplePropertyForm extends ExpandableForm implements PropertyChangeListener {
	private XAttributeSupport support;
	private XModelObject xmo;
	private IWidgetSettings settings;
	private boolean isGreedy = false;
	
	public SamplePropertyForm() {
		settings = new WhiteSettings();
		support = new SampleXAttributeSupport(getSettings());
		support.addPropertyChangeListener(this);
	}
	
	public void dispose() {
		super.dispose();
		if (support!=null) support.dispose();
		support = null;
	}

	class SampleXAttributeSupport extends XAttributeSupport {
		SampleXAttributeSupport(IWidgetSettings settings) {
			super(settings);
		}
		protected boolean keepGreedy(String name, int index, int greedyCount) {
			return alwaysGreedy.contains(name);
		}
	}

	protected Control  createClientArea(Composite parent, IWidgetSettings factory) {
		Composite composite = new Composite(parent, SWT.NONE);
		settings.setupControl(composite);
		//factory.paintBordersFor(composite);
		GridLayout layout = new GridLayout(2,Boolean.FALSE.booleanValue());
		layout.horizontalSpacing = 5;
		layout.verticalSpacing = 5;
		layout.marginHeight = 5;
		layout.marginWidth = 5;
		composite.setLayout(layout);
		if(isGreedy) {
			composite.setLayoutData(new GridData(GridData.FILL_BOTH));
		}

		String elementType = xmo.getAttributeValue("element type");
		if(elementType != null) {
			Label label = new Label(composite, SWT.NONE);
			GridData gd = new GridData(GridData.FILL_HORIZONTAL);
			gd.horizontalSpan = layout.numColumns;
			label.setLayoutData(gd);
			label.setText(elementType);
			label.setBackground(composite.getBackground());
			Font f = label.getFont();
			FontData[] d = f.getFontData();
			for (int i = 0; i < d.length; i++) d[i].setStyle(SWT.BOLD);
			label.setFont(new Font(null, d));			
		}

		updateEnablement();
		support.fillComposite(composite);
		return composite;
	}

	public void initialize(Object model) {
		this.xmo = (XModelObject)model;
		this.model = xmo.getModel();
		XEntityData data = createEntityData();
		if(data != null) {
			support.init(xmo, data, true);
		} else {
			support.init(xmo);
		}
		isGreedy = false;
		XAttribute[] as = xmo.getModelEntity().getAttributes();
		for (int i = 0; i < as.length && !isGreedy; i++) {
			if("always".equals(as[i].getProperty("greedy"))) isGreedy = true;
		}
		updateEnablement();
		this.support.setAutoStore(Boolean.TRUE.booleanValue());
		this.setHeadingText("Properties Editor");
	}
	
	XEntityData createEntityData() {
		if(xmo == null) return null;
		ArrayList<String[]> list = new ArrayList<String[]>();
		list.add(new String[]{xmo.getModelEntity().getName()});
		XAttribute[] as = xmo.getModelEntity().getAttributes();
		for (int i = 0; i < as.length; i++) {
			if("element type".equals(as[i].getName()) || !as[i].isVisible()) continue;
			list.add(new String[]{as[i].getName(), "no"});
		}
		String[][] ss = list.toArray(new String[0][]);
		return XEntityDataImpl.create(ss);		
	}

	public void update() {
		if (support!=null) {
			support.load();
		}
	}
	
	public IWidgetSettings getSettings() {
		if (settings == null) settings = new DefaultSettings();
		return settings;
	}
	
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		updateEnablement();
	}
	
	public void updateEnablement() {
		if (support != null) {
			support.updateEnablementByModelObject();
		}
	}

	public void propertyChange(PropertyChangeEvent evt) {
		updateEnablement();
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.forms.IForm#doGlobalAction(java.lang.String)
	 */
	public boolean doGlobalAction(String actionId) {
		return support.doGlobalAction(actionId);
	}

}
