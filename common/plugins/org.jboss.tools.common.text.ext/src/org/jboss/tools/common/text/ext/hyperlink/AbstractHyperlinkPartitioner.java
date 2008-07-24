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
import java.util.List;

import org.eclipse.jface.text.IDocument;
import org.eclipse.wst.sse.core.internal.provisional.IModelManager;
import org.eclipse.wst.sse.core.internal.provisional.StructuredModelManager;
import org.eclipse.wst.sse.ui.internal.util.Sorter;
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

        List<IHyperlinkPartitioner> partitioners = new ArrayList<IHyperlinkPartitioner>();
        for(int i=0; i<hyperlinkPartitionerDefinitions.length; i++) {
            HyperlinkPartitionerDefinition def = hyperlinkPartitionerDefinitions[i];
            IHyperlinkPartitioner partitioner = def.createHyperlinkPartitioner();
            if (partitioner != null)
            	partitioners.add(partitioner);
        }
        IHyperlinkPartitioner[] sortedPartitioners = orderHyperlinkPartitioners(partitioners.toArray(new IHyperlinkPartitioner[0])); 
        
        for(int i=0; sortedPartitioners != null && i<sortedPartitioners.length; i++) {
            IHyperlinkPartitioner partitioner = sortedPartitioners[i];
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

	protected Sorter createSorter() {
		return new Sorter() {
			public boolean compare(Object partitioner1, Object partitioner2) {
				IHyperlinkPartitioner p1 = (IHyperlinkPartitioner)partitioner1;
				IHyperlinkPartitioner p2 = (IHyperlinkPartitioner)partitioner2;

				String s1 = ((IHyperlinkPartitioner) partitioner1).getClass().getName();
				String s2 = ((IHyperlinkPartitioner) partitioner2).getClass().getName();

				int pp1 = Integer.MAX_VALUE;
				int pp2 = Integer.MAX_VALUE;
				
				if (p1 instanceof IHyperLinkPartitionPriority)
					pp1 = ((IHyperLinkPartitionPriority)p1).getPriority();

				if (p2 instanceof IHyperLinkPartitionPriority)
					pp2 = ((IHyperLinkPartitionPriority)p2).getPriority();
				
				if (pp1 == pp2) {
					return s2.compareTo(s1) > 0;
				}

				return (pp1 < pp2);
			}
		};
	}

	protected IHyperlinkPartitioner[] orderHyperlinkPartitioners(IHyperlinkPartitioner[] partitioners) {
		Object[] sorted = createSorter().sort(partitioners);
		IHyperlinkPartitioner[] sortedPartitioners = new IHyperlinkPartitioner[sorted.length];
		System.arraycopy(sorted, 0, sortedPartitioners, 0, sorted.length);
		return sortedPartitioners;
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
		} finally {
			smw.dispose();
		}
	}

}