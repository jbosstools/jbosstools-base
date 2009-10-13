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
package org.jboss.tools.common.model.ui.editors.dnd;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import org.eclipse.compare.Splitter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;

import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite;
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite.AttributeDescriptorValue;

/**
 * 
 * @author eskimo
 *
 */
public class TagAttributesWizardPage extends DefaultDropWizardPage implements PropertyChangeListener {

	/**
	 * 
	 *
	 */
	public TagAttributesWizardPage() {
		super("Edit required and preferred attributes",""); //$NON-NLS-2$
	}

	/**
	 * 
	 * @param pageName
	 */
	protected TagAttributesWizardPage(String pageName) {
		super(pageName,""); //$NON-NLS-1$
	}

	private TabItem general = null;
	private TabFolder tabs = null;
	private TagAttributesComposite advancedTabContentOnly = null;
	private Splitter composite = null;
	/**
	 * 
	 */
	public void createControl(Composite parent) {
		showAttributes(parent);
	}
	protected void showAttributes(Composite parent) {
		composite =  new Splitter(parent,SWT.HORIZONTAL);

		GridLayout layout = new GridLayout();
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		layout.numColumns = 1;
		composite.setLayout(layout);

		GridData data = new GridData(GridData.FILL_BOTH);
		composite.setLayoutData(data);
		
		tabs = new TabFolder(composite,SWT.TOP);
		tabs.setLayoutData(data);
		general = new TabItem(tabs,SWT.NONE);
		general.setText(DropWizardMessages.General_Tab_Title);		
		final TagAttributesComposite generalTabContent = new TagAttributesComposite(tabs,SWT.NONE,getSpecificWizard().getWizardModel(),true);
		general.setControl(generalTabContent);
		TabItem advanced = new TabItem(tabs,SWT.NONE);
		advanced.setText(DropWizardMessages.Advanced_Tab_Title);		
		final TagAttributesComposite advancedTabContent = new TagAttributesComposite(tabs,SWT.NONE,getSpecificWizard().getWizardModel());
		advanced.setControl(advancedTabContent);
		advancedTabContentOnly = new TagAttributesComposite(composite,SWT.NONE,getSpecificWizard().getWizardModel());
		
		composite.setVisible(tabs,showAdvansedTab());
		composite.setVisible(advancedTabContentOnly,!showAdvansedTab());
		tabs.addSelectionListener(generalTabContent);
		tabs.addSelectionListener(advancedTabContent);
		tabs.addDisposeListener(new DisposeListener() {
			public void widgetDisposed(DisposeEvent e) {
				tabs.removeSelectionListener(generalTabContent);
				tabs.removeSelectionListener(advancedTabContent);
				tabs.removeDisposeListener(this);
				
			}
		});
		setControl(composite);
		getSpecificWizard().getWizardModel().addPropertyChangeListener(IDropWizardModel.TAG_PROPOSAL,this);
		updateTitle();
		runValidation();
	}
	
	/**
	 * 
	 */
	public void propertyChange(PropertyChangeEvent evt) {
		if(evt.getNewValue()!=IDropWizardModel.UNDEFINED_TAG_PROPOSAL) {
			updateTitle();
			composite.setVisible(tabs,showAdvansedTab());
			composite.setVisible(advancedTabContentOnly,!showAdvansedTab());
			composite.layout();
		}
		getControl().redraw();
	}
	
	/**
	 * 
	 *
	 */
	protected void updateTitle() {
		ITagProposal tagProposal = getDropWizardModel().getTagProposal();
		StringBuffer titleText = new StringBuffer();
		titleText.append("<"); //$NON-NLS-1$
		if(!ITagProposal.EMPTY_PREFIX.equals(tagProposal.getPrefix())) {
			titleText
				.append(tagProposal.getPrefix())
				.append(":"); //$NON-NLS-1$
		}
		titleText
			.append(tagProposal.getName())
			.append("> attributes");			 //$NON-NLS-1$
		setTitle(titleText.toString());
	}

	/*
	 * 
	 */
	protected boolean showAdvansedTab() {
		AttributeDescriptorValue[] values = getDropWizardModel().getAttributeValueDescriptors();
		for(int i=0;i<values.length;i++) {
			if(values[i].isPreferable() || values[i].isRequired()) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * 
	 */
	public void validate() throws ValidationException {
	/*	IDropWizardModel model = getSpecificWizard().getWizardModel();
		AttributeDescriptorValue[] values =  model.getAttributeValueDescriptors();
		
		for(int i=0;i<values.length;i++) {
			
			if(values[i].isRequired()) {
				String value = values[i].getValue()==null?"":values[i].getValue().toString();
				if("".equals(value.trim()))
					throw new ValidationException("Attribute '" + values[i].getName() + "' is required");
			}
		}*/
	}

	public void dispose() {
		getSpecificWizard().getWizardModel().removePropertyChangeListener(IDropWizardModel.TAG_PROPOSAL,this);
		super.dispose();
	}
}
