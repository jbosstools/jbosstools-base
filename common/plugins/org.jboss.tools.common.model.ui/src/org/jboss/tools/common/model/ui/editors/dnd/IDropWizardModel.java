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
import org.jboss.tools.common.model.ui.editors.dnd.composite.TagAttributesComposite.AttributeDescriptorValue;

/**
 * 
 * @author eskimo
 */
public interface IDropWizardModel {
	/*
	 * Constants
	 */
	
	public static final String TAG_PROPOSAL = "tagProposal";
	
	public static final String ATTRIBUTE_VALUE = "attributeValue";	
	
	public static final TagProposal UNDEFINED_TAG_PROPOSAL = new TagProposal("http://www.redhat.com/undefined","","");	

	/*
	 * Methods 
	 */
	
	public TagProposal getTagProposal();

	public void setTagProposal(TagProposal selection);

	public void setAttributeValue(AttributeDescriptorValue name, Object value);
	
	public void setAttributeValue(String name, Object value);
	
	public AttributeDescriptorValue[] getAttributeValueDescriptors();
	
	public DropData getDropData();

	public void setDropData(DropData dropData);

	public boolean isWizardRequired();	
	
	public boolean isValid();
	
	public void addPropertyChangeListener(PropertyChangeListener listener);
	
	public void removePropertyChangeListener(PropertyChangeListener listener);
	
	public void addPropertyChangeListener(String propertyName, PropertyChangeListener listener);
	
	public void removePropertyChangeListener(String propertyName,PropertyChangeListener listener);
	
	public ITagProposalFactory getTagProposalFactory();

	/**
	 * @param prompt if 'true' always prompt for tag attributes during tag insert. 
	 */
	public void setPromptForTagAttributesRequired(boolean prompt);

	/**
	 * @return 'true' if prompt for tag attributes during tag insert always is required.
	 */
	public boolean isPromptForTagAttributesRequired();
}