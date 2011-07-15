/******************************************************************************* 
 * Copyright (c) 2010 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.resolver;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.text.ITextSourceReference;

/**
 * @author Daniel Azarov
 */
public class MessagePropertyELSegmentImpl extends ELSegmentImpl implements
		MessagePropertyELSegment {
	
	private IFile messageBundleResource = null;
	private ITextSourceReference messagePropertySourceReference = null;
	private int propertyStart=0, propertyLength=0;
	private String baseName=null;
	private boolean isBundle = false;

	public MessagePropertyELSegmentImpl(LexicalToken token) {
		super(token);
	}

	public IFile getMessageBundleResource() {
		return messageBundleResource;
	}
	
	public void setMessageBundleResource(IFile resource){
		messageBundleResource = resource;
		setResource(resource);
	}

	public void setBundleOnlySegment(boolean set) {
		isBundle = set;
	}
	
	public boolean isBundle() {
		return isBundle;
	}
	
	public boolean isProperty() {
		return !isBundle && (messagePropertySourceReference != null);
	}

	public ITextSourceReference getMessagePropertySourceReference() {
		if(messagePropertySourceReference==null) {
			messagePropertySourceReference = new ITextSourceReference() {
				public int getStartPosition() {
					return propertyStart;
				}
				public int getLength() {
					return propertyLength;
				}
				public IResource getResource() {
					return resource;
				}
			};
		}
		return messagePropertySourceReference;
	}
	
	public void setMessagePropertySourceReference(int start, int lenght) {
		propertyStart = start;
		propertyLength = lenght;
	}
	
	public String getBaseName(){
		return baseName;
	}
	
	public void setBaseName(String name){
		baseName = name;
	}
}