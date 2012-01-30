/*******************************************************************************
 * Copyright (c) 2001, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Jens Lukowski/Innoopract - initial renaming/restructuring
 *     Exadel, Inc.
 *     Red Hat, Inc.     
 *******************************************************************************/
package org.jboss.tools.common.text.ext.hyperlink.xpl;

import java.util.ArrayList;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.hyperlink.IHyperlink;
import org.eclipse.jface.text.hyperlink.IHyperlinkDetector;
import org.eclipse.wst.sse.core.internal.provisional.IModelManager;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.StructuredModelManager;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkBuilder;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkDetector;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;

public class BaseHyperlinkDetector implements IHyperlinkDetector{

	public IHyperlink[] getHyperlinks(ITextViewer textViewer, IRegion region) {
	    IHyperlink[] hyperlinks = null;

		// determine the current partition
		if (textViewer != null && textViewer.getDocument() != null) {
			String contentType = getContentType(textViewer.getDocument());
			IHyperlinkRegion partition = getPartition(textViewer.getDocument(), region.getOffset());
			hyperlinks = partition == null ? null : getHyperlinks(textViewer, region, contentType, partition);
		}
		return hyperlinks;
	}

	/**
	 * Returns IHyperlink array for the document and region type.
	 */
	public IHyperlink[] getHyperlinks(ITextViewer textViewer, IRegion region, String contentType, IHyperlinkRegion partition) {
	    ArrayList<IHyperlink> hyperlinks = new ArrayList<IHyperlink>();
		// determine the current partition
		if (textViewer != null && textViewer.getDocument() != null) {
			// query HyperlinkBuilder and get the list of open ons for the
			// current partition
			HyperlinkDefinition[] defs = HyperlinkBuilder.getInstance().getHyperlinkDefinitions(contentType, partition.getType());

			if(defs==null) return null;

			for(int i=0; i<defs.length; i++) {
			    IHyperlink hyperlink = defs[i].createHyperlink();
			    if(!hyperlinks.contains(hyperlink)) {
			    	if (hyperlink instanceof AbstractHyperlink) {
			    		((AbstractHyperlink)hyperlink).setDocument(textViewer.getDocument());
			    		((AbstractHyperlink)hyperlink).setOffset(region.getOffset());
			    		((AbstractHyperlink)hyperlink).setRegion(partition);
			    	}
				    hyperlinks.add(hyperlink);			        
			    }
			}
		}
		return (hyperlinks.size() == 0 ? null : (IHyperlink[])hyperlinks.toArray(new IHyperlink[hyperlinks.size()]));
	}

	/**
	 * Returns the content type of document
	 * 
	 * @param document -
	 *            assumes document is not null
	 * @return String content type of given document
	 */
	protected String getContentType(IDocument document) {
		String type = null;

		IModelManager mgr = StructuredModelManager.getModelManager();
		IStructuredModel model = null;
		try {
			model = mgr.getExistingModelForRead(document);
			if (model != null) {
				type = model.getContentTypeIdentifier();
			}
		} finally {
			if (model != null) {
				model.releaseFromRead();
			}
		}
		return type;
	}

	/**
	 * Returns the partition type located at offset in the document
	 * 
	 * @param document -
	 *            assumes document is not null
	 * @param offset
	 * @return String partition type
	 */
	protected IHyperlinkRegion getPartition(IDocument document, int offset) {
	    String type = null;
	    IHyperlinkRegion[] regions = getPartitions(document, offset);
	    // if more than 1 hyperlink partitioner type is returned just returning the first one.
		return (regions != null && regions.length > 0 ? regions[0] : null);
	}

	/**
	 * 
	 */
	public IHyperlink[] detectHyperlinks(ITextViewer textViewer, IRegion region, boolean canShowMultipleHyperlinks) {
	    IHyperlink[] hyperlinks = getHyperlinks(textViewer, region);
	    // if more than 1 hyperlink is returned just returning the first one.
		if (hyperlinks != null && hyperlinks.length > 0) {
			if (!canShowMultipleHyperlinks) {
				hyperlinks = new IHyperlink[] {hyperlinks[0]}; 
			}
		}
		return hyperlinks;
	}	
	
	protected IHyperlinkRegion[] getPartitions(IDocument document, int offset) {
		return new IHyperlinkRegion[0];
	}
}
