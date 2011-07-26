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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.util.FindObjectHelper;
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

	private List<XModelObject> objects = new ArrayList<XModelObject>();

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

	public void addObject(XModelObject object) {
		objects.add(object);
	}

	public IOpenableReference[] getOpenable() {
		if(objects != null) {
			IOpenableReference[] result = new IOpenableReference[objects.size()];
			for (int i = 0; i < objects.size(); i++) {
				final XModelObject o = objects.get(i);
				result[i] = new IOpenableReference() {
					@Override
					public boolean open() {
						int q = FindObjectHelper.findModelObject(o, FindObjectHelper.IN_EDITOR_ONLY);
						return  q < 1;
					}
					
					@Override
					public String getLabel() {
						XModelObject p = o;
						while(p != null && p.getFileType() < XModelObject.FILE) p = p.getParent();
						//improve label - now it returns file name, with locale
						return p.getAttributeValue("name");
					}
					
					@Override
					public Image getImage() {
						return null;
					}
				};
			}
			return result;
		}
		return super.getOpenable();
	}
}