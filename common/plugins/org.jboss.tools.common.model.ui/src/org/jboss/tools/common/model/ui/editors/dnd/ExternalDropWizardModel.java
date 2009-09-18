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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import org.jboss.tools.common.model.ui.editors.dnd.DropUtils.AttributeDescriptorValueProvider;
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite.AttributeDescriptorValue;

public class ExternalDropWizardModel extends DefaultDropWizardModel implements IDropWizardModel {

	public ExternalDropWizardModel(ITagProposalFactory tagProposalFactory) {
		super(tagProposalFactory);
	}

	/**
	 * @see org.jboss.tools.vpe.editor.dnd.IDnDWizardDataModel#getAttributeNameList()
	 */
	public List getAttributeNameList() {
		return new ArrayList();
	}
	
	/**
	 * @see org.jboss.tools.vpe.editor.dnd.IDnDWizardDataModel#getAttributeValue(java.lang.String)
	 */
	public String getAttributeValue(String name) {
		return null;
	}

	protected void doLoadTagAttributeDescriptors() {
		if(getTagProposal()!=UNDEFINED_TAG_PROPOSAL) {
			DropData data = getDropData();
			ITagProposal proposal = getTagProposal();
			AttributeDescriptorValueProvider valueProvider = data.getValueProvider();
			if(valueProvider != null) valueProvider.setProposal(proposal);
			AttributeDescriptorValue[] values = valueProvider == null ? new AttributeDescriptorValue[0]
			                                  : valueProvider.getValues();
				
			fAttributeValues = new ArrayList<AttributeDescriptorValue>(Arrays.asList(sort(values)));
		} else {
			fAttributeValues = new ArrayList<AttributeDescriptorValue>();
		}
	}
	
	/*
	 * Sorting AttributeDescriptorValue in below manner:
	 * - required atttributes
	 * - preferred attributes
	 * - all others filled
	 * - all others not filled
	 */
	private AttributeDescriptorValue[] sort(AttributeDescriptorValue[] elements) {
		Arrays.sort(
			elements, 
			new Comparator<AttributeDescriptorValue>() {
				public int compare(AttributeDescriptorValue arg0, AttributeDescriptorValue arg1) {

					AttributeDescriptorValue value1	= arg0;
					AttributeDescriptorValue value2	= arg1;

					if(value1.isRequired() && value2.isRequired()) {
						return value1.getName().compareTo(value2.getName());
					} else if(value1.isRequired()){
						return -1;
					} else if(value2.isRequired()){
						return 1;
					} else {
						if(value1.isPreferable() && value2.isPreferable()) {
							return value1.getName().compareTo(value2.getName());
						} else if(value1.isPreferable()){
							return -1;
						} else if(value2.isPreferable()){
							return 1;
						} else {
							return value1.getName().compareTo(value2.getName());
						}
					}
				}
			}
		);
		return elements;
	}	

}
