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

import org.eclipse.jface.text.IDocument;
import org.eclipse.wst.sse.core.internal.provisional.IModelManager;
import org.eclipse.wst.sse.core.internal.provisional.StructuredModelManager;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 * @author Igels
 */
public abstract class AbstractHyperlinkPartitioner implements IHyperlinkPartitioner {

    public String getChildPartitionType(IDocument document, IHyperlinkRegion superRegion) {
        IHyperlinkRegion childRegion = parse(document, superRegion);
        if (childRegion == null) return null;
        HyperlinkPartitionerDefinition[] hyperlinkPartitionerDefinitions = HyperlinkPartitionerBuilder.getInstance().getHyperlinkPartitionerDefinitions(childRegion.getContentType(), childRegion.getType(), childRegion.getAxis());
        for(int i=0; i<hyperlinkPartitionerDefinitions.length; i++) {
            HyperlinkPartitionerDefinition def = hyperlinkPartitionerDefinitions[i];
            IHyperlinkPartitioner partitioner = def.createHyperlinkPartitioner();
            if((!(partitioner instanceof IHyperlinkPartitionRecognizer)) || 
            		((IHyperlinkPartitionRecognizer)partitioner).recognize(document, childRegion)) {
            	if (partitioner instanceof IExclusiblePartitionerRecognition) {
            		IExclusiblePartitionerRecognition epr = (IExclusiblePartitionerRecognition)partitioner;
            		IHyperlinkPartitioner replacement = findExclusionPartitioner(epr.getExclusionPartitionType(), hyperlinkPartitionerDefinitions, document, childRegion);
            		if (replacement != null)
            			return replacement.getChildPartitionType(document, childRegion);
            	}
                return partitioner.getChildPartitionType(document, childRegion);
            }
        }

        return childRegion.getType();
    }

    IHyperlinkPartitioner findExclusionPartitioner (String partitionType, HyperlinkPartitionerDefinition[] hyperlinkPartitionerDefinitions, IDocument document, IHyperlinkRegion region) {
        for(int i=0; i<hyperlinkPartitionerDefinitions.length; i++) {
            HyperlinkPartitionerDefinition def = hyperlinkPartitionerDefinitions[i];
            IHyperlinkPartitioner partitioner = def.createHyperlinkPartitioner();
            if((partitioner instanceof IExclusiblePartitionerRecognition) && 
            		((IExclusiblePartitionerRecognition)partitioner).excludes(partitionType, document, region)) {
                return partitioner;
            }
        }
        return null;
    }
    
	protected static IModelManager getModelManager() {
		return StructuredModelManager.getModelManager();
	}

    protected abstract IHyperlinkRegion parse(IDocument document, IHyperlinkRegion superRegion);

	protected String getAxis(IDocument document, IHyperlinkRegion superRegion) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			Node node = Utils.findNodeForOffset(xmlDocument, superRegion.getOffset());
			if (node instanceof Attr) {
				Attr attr = (Attr)node;
				return Utils.getParentAxisForNode(xmlDocument, attr) + attr.getName() + "/";
			}
			return Utils.getParentAxisForNode(xmlDocument, node);
		} catch (Exception x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

}