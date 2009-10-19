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

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.List;

import org.jboss.tools.common.model.ui.editors.dnd.composite.TagProposalsComposite;
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite.AttributeDescriptorValue;

public class DefaultDropWizardModel implements IDropWizardModel {

	ITagProposal fSelectedTagProp = UNDEFINED_TAG_PROPOSAL;
	DropData fDropData;
	List<AttributeDescriptorValue> fAttributeValues = new ArrayList<AttributeDescriptorValue>();
	boolean promptForTagAttributes = true;

	ITagProposalFactory tagProposalFactory; 

	public DefaultDropWizardModel(ITagProposalFactory tagProposalFactory) {
		this.tagProposalFactory = tagProposalFactory;
	}

	public ITagProposal getTagProposal() {
		return fSelectedTagProp;
	}

	/**
	 * @see org.jboss.tools.vpe.editor.dnd.IDnDWizardDataModel#setTagProposal(org.jboss.tools.vpe.editor.dnd.TagProposal)
	 */
	public void setTagProposal(ITagProposal selection) {
		ITagProposal oldValue = fSelectedTagProp;
		fSelectedTagProp = selection;
		doLoadTagAttributeDescriptors();
		doLoadDefaultAttributeValues();		
		fireModelChaged(TAG_PROPOSAL,oldValue,fSelectedTagProp);
	}

	/**
	 * 
	 */
	public void setAttributeValue(AttributeDescriptorValue descrValue, Object value) {
//		Object oldValue = descrValue.getValue();
 		descrValue.setValue(value);
		fireModelChaged(ATTRIBUTE_VALUE,null,descrValue);
	}

	/**
	 * 
	 */
	public void setAttributeValue(String name, Object value) {
		if(name==null) throw new IllegalArgumentException("Attribute name cannot be null"); //$NON-NLS-1$
		AttributeDescriptorValue descrValue = findDescriptor(name);
		if(descrValue==null) {
			throw new IllegalArgumentException("Attribute '" + name + "' not found"); //$NON-NLS-1$ //$NON-NLS-2$
		}
 		descrValue.setValue(value);
		fireModelChaged(ATTRIBUTE_VALUE,null,descrValue);
	}

	public void setPreferable(String name) {
		if(name==null) return;
		AttributeDescriptorValue descrValue = findDescriptor(name);
		if(descrValue != null) descrValue.setPreferable(true);
	}

	private AttributeDescriptorValue findDescriptor(String name) {
		for (int i = 0; i < fAttributeValues.size(); i++) {
			AttributeDescriptorValue arrayElement = (AttributeDescriptorValue)fAttributeValues.get(i);
				if(name.equals(arrayElement.getName())) {
					return arrayElement;
			}
		}
		return null;
	}

	/**
	 * 
	 */
	public List getAttributeNameList() {
		return null;
	}

	/**
	 * 
	 */
	public String getAttributeValue(String name) {
		return null;
	}

	/**
	 * 
	 */
	public void setDropData(DropData data) {
		fDropData = data;
	}

	/**
	 * 
	 */
	public DropData getDropData() {
		return fDropData;
	}

	/**
	 * 
	 */
	public AttributeDescriptorValue[] getAttributeValueDescriptors() {
		return (AttributeDescriptorValue[])fAttributeValues.toArray(new AttributeDescriptorValue[fAttributeValues.size()]);
	}

	private void doLoadDefaultAttributeValues() {
		getTagProposal().getAttributesValueLoader().fillTagAttributes(this);
	}

	PropertyChangeSupport pcs = new PropertyChangeSupport(this);

	/**
	 * 
	 */
	public void addPropertyChangeListener(PropertyChangeListener listener) {
		pcs.addPropertyChangeListener(listener);
	}

	/**
	 * 
	 */
	public void removePropertyChangeListener(PropertyChangeListener listener) {
		pcs.removePropertyChangeListener(listener);		
	}

	/**
	 * 
	 * @param propertyName
	 * @param oldValue
	 * @param newValue
	 */
	private void fireModelChaged(String propertyName, Object oldValue, Object newValue) {
		pcs.firePropertyChange(propertyName,oldValue,newValue);
	}

	/**
	 * 
	 *
	 */
	protected void doLoadTagAttributeDescriptors() {
	}

	/**
	 * 
	 */
	public void addPropertyChangeListener(String propertyName, PropertyChangeListener listener) {
		pcs.addPropertyChangeListener(propertyName,listener);
	}

	/**
	 * 
	 */
	public void removePropertyChangeListener(String propertyName, PropertyChangeListener listener) {
		pcs.removePropertyChangeListener(propertyName,listener);		
	}

	/**
	 * Wizard required if there is:
	 * - required or preffered attributes (and VPE preference "always prompt for tag attributes during tag insert" = true)
	 * - more the one tag proposal.
	 */
	public boolean isWizardRequired() {
		if(getTagProposal()!=UNDEFINED_TAG_PROPOSAL && getAttributeValueDescriptors().length==0) {
			return false;
		} else if(getTagProposal()!=UNDEFINED_TAG_PROPOSAL && getAttributeValueDescriptors().length>0){
			AttributeDescriptorValue[] values = getAttributeValueDescriptors();
			for (int i = 0; i < values.length; i++) {
				AttributeDescriptorValue element = values[i];
				if(element.isPreferable() || element.isRequired()) {
					return true;
				}
			}
			if(values.length > 5) return true;
			return isPromptForTagAttributesRequired();
		} else if(getTagProposal()==UNDEFINED_TAG_PROPOSAL){
			ITagProposal[] proposals 
				= TagProposalsComposite.getTagProposals(
					getDropData().getMimeType(),
					getDropData(),
					tagProposalFactory
				);
			if(proposals.length>1) {
				return true;
			}
		}
		return false;
	}

	public boolean isValid() {
		if(getTagProposal()==IDropWizardModel.UNDEFINED_TAG_PROPOSAL) {
			return false;
		}
		return true;
	}

	public ITagProposalFactory getTagProposalFactory() {
		return tagProposalFactory;
	}

	public boolean isPromptForTagAttributesRequired() {
		return this.promptForTagAttributes;
	}

	public void setPromptForTagAttributesRequired(boolean prompt) {
		this.promptForTagAttributes = prompt;
	}
}