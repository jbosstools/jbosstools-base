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
package org.jboss.tools.common.text.ext.hyperlink;


import java.util.ArrayList;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.ITypedRegion;
import org.jboss.tools.common.text.ext.hyperlink.xpl.BaseHyperlinkDetector;
import org.jboss.tools.common.text.ext.util.AxisUtil;

public class HyperlinkDetector extends BaseHyperlinkDetector {
    
	private static HyperlinkDetector fInstance;

    /**
	 * returns singleton instance of HyperlinkDetector
	 * 
	 * @return HyperlinkDetector
	 */
	public static HyperlinkDetector getInstance() {
		return HyperlinkDetectorHolder.INSTANCE;
	}
	
	/**
	 * Returns the partition types located at offset in the document
	 * 
	 * @param document -
	 *            assumes document is not null
	 * @param offset
	 * @return String partition types
	 */
	protected String[] getPartitionTypes(IDocument document, int offset) {
		String documentRegionType = null;
		ITypedRegion region = null;
		ArrayList<String> types = new ArrayList<String>();
		
			region = (document instanceof IDocumentExtension3 ? 
					((IDocumentExtension3)document).getDocumentPartitioner("org.eclipse.wst.sse.core.default_structured_text_partitioning").getPartition(offset) :  //$NON-NLS-1$
					document.getDocumentPartitioner().getPartition(offset)); 

		if (region != null) {
		    documentRegionType = region.getType();
		
			String contentType = getContentType(document);
		    
			HyperlinkPartitionerDefinition[] defs = HyperlinkPartitionerBuilder.getInstance().getHyperlinkPartitionerDefinitions(contentType, documentRegionType, null);
	
			if(defs==null || defs.length==0) {
				types.add(documentRegionType);
			} else {
				for(int i=0; i<defs.length; i++) {
				    final ITypedRegion finalDocumentRegion = region;
				    final IDocument finalDocument = document;
				    final String finalContentType = contentType;
				    IHyperlinkPartitioner hyperlinkPartitioner = defs[i].createHyperlinkPartitioner();
				    IHyperlinkRegion startHyperlinkRegion = new HyperlinkRegion(
				    		finalDocumentRegion.getOffset(),
				    		finalDocumentRegion.getLength(),
				    		AxisUtil.getAxis(finalDocument, finalDocumentRegion.getOffset()),
				    		finalContentType,
				    		finalDocumentRegion.getType());
		            if((!(hyperlinkPartitioner instanceof IHyperlinkPartitionRecognizer)) || 
		            		((IHyperlinkPartitionRecognizer)hyperlinkPartitioner).recognize(document, startHyperlinkRegion)) {
		                String type = hyperlinkPartitioner.getChildPartitionType(document, startHyperlinkRegion);
		    		    if(type!=null && !types.contains(type)) {
		    		        types.add(type);			        
		    		    }
		            }
				}
			}
		}
		return types.toArray(new String[types.size()]);
	}

	static class HyperlinkDetectorHolder {
		static HyperlinkDetector INSTANCE = new HyperlinkDetector();	
	}
}