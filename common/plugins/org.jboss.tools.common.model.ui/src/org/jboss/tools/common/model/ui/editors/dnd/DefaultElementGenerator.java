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

import org.jboss.tools.common.kb.TagDescriptor;
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite.AttributeDescriptorValue;

public class DefaultElementGenerator implements IElementGenerator {

	IDropWizardModel fDataModel;
	
	public void setDataModel(Object object) {
		if(object instanceof IDropWizardModel) {
			fDataModel = (IDropWizardModel)object;
			return;
		}
		throw new IllegalArgumentException("Object parametr must be instance of " + this.getClass().getName());
	}

	public IDropWizardModel getWizardDataModel() {
		return fDataModel;
	}
	
	public DropData getDropData() {
		return fDataModel.getDropData();
	}
	
	public String generateStartTag() {
		TagProposal proposal = getWizardDataModel().getTagProposal();
		TagDescriptor descr = DropUtils.getJspTagDescriptor(
				getWizardDataModel().getDropData().getSourceViewer().getDocument(),
				proposal.getUri(),
				proposal.getLibraryVersion(),
				proposal.getPrefix(),
				proposal.getName()
			);
			StringBuffer tagText = new StringBuffer();
			 
			if(descr!=null) {
				String fullName = descr.getFullName();
				if(descr.getPrefix()==null) {
					// for HTML
					fullName = fullName.toLowerCase();
				}
				tagText.append("<" + applayTagPreferences(fullName));			
				AttributeDescriptorValue[] values = getWizardDataModel().getAttributeValueDescriptors();
				for(int i=0;i<values.length;i++) {
					Object value = values[i].getValue();
					if(value != null && !"".equals(value.toString().trim())) {
						tagText
							.append(" ")
							.append(applayAttributePreferences(values[i].getName()))
							.append("=")
							.append("\"")
							.append(value.toString())
							.append("\"");
					}
				}
				
				if(descr.hasClosingTag()) {
					if(descr.hasBody()) {
						tagText
							.append(">")
							.append("</")
							.append(fullName);
					} else {
						tagText
							.append("/");
					}
				} 
				tagText			
					.append(">");

			} else {
				tagText
					.append("<");
				
				if(getWizardDataModel().getTagProposal().getPrefix()!=null 
						&& !"".equals(getWizardDataModel().getTagProposal().getPrefix().trim())) {
					tagText.append(getWizardDataModel().getTagProposal().getPrefix())
						.append(":");
				}
				
				tagText
					.append(getWizardDataModel().getTagProposal().getName())
					.append("/>");
			}
			return tagText.toString();

	}

	public String generateEndTag() {
		return "";
	}
	
	protected String applayAttributePreferences(String attribute) {
		return attribute;
	}
	
	protected String applayTagPreferences(String tagName) {
		return tagName;
	}
}
