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

/**
 * 
 */

import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.ui.IEditorInput;
import org.w3c.dom.Node;

import org.jboss.tools.common.model.ui.editors.dnd.DropUtils.AttributeDescriptorValueProvider;

public class DropData {
	
	private IEditorInput fInput;
	private ISourceViewer fViewer; 
	private ISelectionProvider fProvider;	
	private String fMimeType;
	private String fMimeData;
	private IDropCommand fDropCommand = null;
	private Node container = null;
	private String attributeName = null;
	private AttributeDescriptorValueProvider valueProvider;
	
	/**
	 * 
	 * @param mimeType
	 * @param data
	 * @param pageContext
	 * @param input
	 * @param viewer
	 * @param provider
	 */
	public DropData(
			String mimeType,
			String data,
			IEditorInput input, 
			ISourceViewer viewer, 
			ISelectionProvider provider,
			Node container
		) {
		this(mimeType, data, input, viewer, provider);
		this.container = container;
	}

	/**
	 * 
	 * @param mimeType
	 * @param mimeData
	 * @param pageContext
	 * @param input
	 * @param viewer
	 * @param provider
	 */
	public DropData(
		String mimeType,
		String mimeData,
		IEditorInput input, 
		ISourceViewer viewer, 
		ISelectionProvider provider
	) {
		fMimeType = mimeType;
		fMimeData = mimeData;
		fInput = input;
		fViewer = viewer;
		fProvider = provider;
//		this.pageContext = pageContext;
	}
	
	public void setAttributeName(String n) {
		attributeName = n;
	}
	
	public String getAttributeName() {
		return attributeName;
	}

	/**
	 * 
	 */
	public String getMimeData() {
		return fMimeData;
	}

	/**
	 * 
	 */
	public void setMimeData(String mimeData) {
		fMimeData = mimeData;
	}

	/**
	 * 
	 */
	public IDropCommand getDropCommand() {
		return fDropCommand;
	}

	/**
	 * 
	 */
	public void setDropCommand(IDropCommand dropCommand) {
		fDropCommand = dropCommand;
	}

	/**
	 * 
	 */
	public IEditorInput getEditorInput() {
		return fInput;
	}

	/**
	 * 
	 */
	public void setEditorInput(IEditorInput input) {
		fInput = input;
	}

	/**
	 * 
	 */
	public String getMimeType() {
		return fMimeType;
	}

	/**
	 * 
	 */
	public void setMimeType(String mimeType) {
		fMimeType = mimeType;
	}

	/**
	 * 
	 */
	public ISelectionProvider getSelectionProvider() {
		return fProvider;
	}

	/**
	 * 
	 */
	public void setSelectionProvider(ISelectionProvider provider) {
		fProvider = provider;
	}

	/**
	 * 
	 */
	public ISourceViewer getSourceViewer() {
		return fViewer;
	}

	/**
	 * 
	 */
	public void setSourceViewer(ISourceViewer viewer) {
		fViewer = viewer;
	}

	public Node getContainer() {
		return container;
	}

	public void setValueProvider(AttributeDescriptorValueProvider valueProvider) {
		this.valueProvider = valueProvider;	
	}

	public AttributeDescriptorValueProvider getValueProvider() {
		return valueProvider;
	}

}
