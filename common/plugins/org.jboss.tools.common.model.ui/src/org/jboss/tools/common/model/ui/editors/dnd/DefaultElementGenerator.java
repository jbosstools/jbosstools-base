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

import org.jboss.tools.common.model.ui.editors.dnd.DropUtils.AttributeDescriptorValueProvider;
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite.AttributeDescriptorValue;

public class DefaultElementGenerator implements IElementGenerator {

	IDropWizardModel fDataModel;
	
	public void setDataModel(Object object) {
		if(object instanceof IDropWizardModel) {
			fDataModel = (IDropWizardModel)object;
			return;
		}
		throw new IllegalArgumentException("Object parametr must be instance of " + this.getClass().getName()); //$NON-NLS-1$
	}

	public IDropWizardModel getWizardDataModel() {
		return fDataModel;
	}
	
	public DropData getDropData() {
		return fDataModel.getDropData();
	}
	
	public String generateStartTag() {
		ITagProposal proposal = getWizardDataModel().getTagProposal();
		AttributeDescriptorValueProvider valueProvider = getWizardDataModel().getDropData().getValueProvider();
		if(valueProvider != null) valueProvider.setProposal(proposal);
		String tagName = valueProvider == null ? null : valueProvider.getTag();
			StringBuffer tagText = new StringBuffer();
			 
			if(tagName != null) {
				String fullName = tagName;
				if(tagName.indexOf(':') < 0) {
					// for HTML
					fullName = fullName.toLowerCase();
				}
				tagText.append("<" + applayTagPreferences(fullName));			 //$NON-NLS-1$
				AttributeDescriptorValue[] values = getWizardDataModel().getAttributeValueDescriptors();
				for(int i=0;i<values.length;i++) {
					Object value = values[i].getValue();
					if(value != null && !"".equals(value.toString().trim())) { //$NON-NLS-1$
						tagText
							.append(" ") //$NON-NLS-1$
							.append(applayAttributePreferences(values[i].getName()))
							.append("=") //$NON-NLS-1$
							.append("\"") //$NON-NLS-1$
							.append(value.toString())
							.append("\""); //$NON-NLS-1$
					}
				}
				
				if(tagName.indexOf(':') > 0) { //TODO html case, old kb had specific method
					if(valueProvider.canHaveBody()) {
						tagText
							.append(">") //$NON-NLS-1$
							.append("</") //$NON-NLS-1$
							.append(fullName);
					} else {
						tagText
							.append("/"); //$NON-NLS-1$
					}
				} 
				tagText			
					.append(">"); //$NON-NLS-1$

			} else {
				tagText
					.append("<"); //$NON-NLS-1$
				
				if(getWizardDataModel().getTagProposal().getPrefix()!=null 
						&& !"".equals(getWizardDataModel().getTagProposal().getPrefix().trim())) { //$NON-NLS-1$
					tagText.append(getWizardDataModel().getTagProposal().getPrefix())
						.append(":"); //$NON-NLS-1$
				}
				
				tagText
					.append(getWizardDataModel().getTagProposal().getName())
					.append("/>"); //$NON-NLS-1$
			}
			return tagText.toString();

	}

	public String generateEndTag() {
		return ""; //$NON-NLS-1$
	}
	
	protected String applayAttributePreferences(String attribute) {
		return attribute;
	}
	
	protected String applayTagPreferences(String tagName) {
		return tagName;
	}
}
