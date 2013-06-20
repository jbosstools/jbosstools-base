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
		throw new IllegalArgumentException("Object parameter must be instance of " + this.getClass().getName()); //$NON-NLS-1$
	}

	public IDropWizardModel getWizardDataModel() {
		return fDataModel;
	}
	
	public DropData getDropData() {
		return fDataModel.getDropData();
	}

	protected void generateChildren(ElementNode node) {
	}

	protected ElementNode generateRoot() {
		ElementNode root = RootNode.newRoot();
		generateNode(root);
		return root;
	}

	protected void generateNode(ElementNode root) {
		ITagProposal proposal = getWizardDataModel().getTagProposal();
		AttributeDescriptorValueProvider valueProvider = getWizardDataModel().getDropData().getValueProvider();
		if(valueProvider != null) valueProvider.setProposal(proposal);
		String tagName = valueProvider == null ? null : valueProvider.getTag();
		if(tagName != null) {
			String fullName = tagName;
			if(tagName.indexOf(':') < 0) {
				// for HTML
				fullName = fullName.toLowerCase();
			}
			fullName = applayTagPreferences(fullName);
			ElementNode node = root.addChild(fullName);
			AttributeDescriptorValue[] values = getWizardDataModel().getAttributeValueDescriptors();
			for(int i = 0; i < values.length; i++) {
				Object value = values[i].getValue();
				if(value != null && !"".equals(value.toString().trim())) { //$NON-NLS-1$
					node.addAttribute(applayAttributePreferences(values[i].getName()), value.toString());
				}
			}
			generateChildren(node);
			if(valueProvider.canHaveBody()) {
				node.getChildren().add(SEPARATOR);
				node.empty = false;
			}
			
		} else {
			String prefix = getWizardDataModel().getTagProposal().getPrefix();
			String name = getWizardDataModel().getTagProposal().getName();
			if(prefix != null && prefix.length() > 0) {
				name = prefix + ":" + name;
			}
			root.addChild(name);
		}
		
	}
	
	public String generateStartTag() {
		generatedEndTag = "";
		ElementNode root = generateRoot();
		NodeWriter w = new NodeWriter(true);
		root.flush(w, 0);
		String[] result = w.getResult();
		String startText = result[0];
		generatedEndTag = result.length < 2 ? "" :
			result.length == 3 ? result[1] + "\n" + result[2] : result[1];
		return startText;
	}

	protected String generatedEndTag = ""; //$NON-NLS-1$

	public String generateEndTag() {
		return generatedEndTag;
	}
	
	protected String applayAttributePreferences(String attribute) {
		return attribute;
	}
	
	protected String applayTagPreferences(String tagName) {
		return tagName;
	}
}
