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

import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMElement;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMText;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.util.RegionHolder;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.StructuredSelectionHelper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.jboss.tools.common.text.ext.hyperlink.xml.XMLRootHyperlinkPartitioner;

/**
 * @author Jeremy
 *
 */
public class JumpToHyperlink extends AbstractHyperlink {

	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doHyperlink(org.eclipse.jface.text.IRegion)
	 */
	protected void doHyperlink(IRegion region) {
	
		try {
			RegionHolder holder = getRegionHolder(getName(region), region);
			if (holder != null) {
				StructuredSelectionHelper.setSelectionAndRevealInActiveEditor(holder.region);
			} else {
				openFileFailed();
			}
			
		} catch (Exception x) {
			// could not open editor
			openFileFailed();
		}
	}

	protected String getName(IRegion region) {
		try {
			return Utils.trimQuotes(getDocument().get(region.getOffset(), region.getLength()));
		} catch (Exception x) {
			//ignore
			return null;
		}
	}

	protected String getDestinationAxis() {
		return null;
	}
	
	protected NodeList getRootElementsToSearch (IRegion region) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;

			return xmlDocument.getChildNodes();
		} catch (Exception x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}

	protected RegionHolder getRegionHolder(String content, IRegion region) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;

			List elements = findElementsByAxis(getRootElementsToSearch(region), getDestinationAxis());
			
			if (elements == null || elements.size() == 0) return null;
			
			for (int i = elements.size() - 1; i >= 0; i--) {
				if (elements.get(i) instanceof IDOMElement) {
					IDOMElement element = (IDOMElement)elements.get(i);
					String text = Utils.trimQuotes(getElementText(element));
					if (content.equals(text)) {
						return new RegionHolder(new Region (element.getStartOffset(), 
								element.getStartStructuredDocumentRegion().getLength()));
					}
				}
			}
			return null;
		} catch (Exception x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
	}
	
	protected String getElementText(Node element) {
		try {
			if (element instanceof IDOMText) 
				return ((IDOMText)element).getData();

			StringBuffer text = new StringBuffer();
			if (element instanceof IDOMElement) {
				Node child = element.getFirstChild();
				while (child != null) {
					if (child instanceof IDOMText) {
						text.append(((IDOMText)child).getData());
					}
					child = child.getNextSibling();
				}
			}
			String result = text.toString();
			if (result == null || result.length() == 0) return null;
			return result;
		} catch (Exception x) {
			//ignore
			return null;
		}
	}
	
	protected List<Node> findElementsByAxis (NodeList list, String axis) {
		String requiredAxis = axis.toLowerCase();
		List<Node> elements = new ArrayList<Node>();
		try {
			for (int i = 0; list != null && i < list.getLength(); i++) {
				try {
					IDOMElement element = (IDOMElement)list.item(i);
					String currentAxis = XMLRootHyperlinkPartitioner.computeAxis(getDocument(), element.getStartOffset()) + "/";
					currentAxis  = currentAxis.toLowerCase();
					
					if (currentAxis.endsWith(requiredAxis)) {
						elements.add(element);
					}
					
					if (element.hasChildNodes()) {
						List<Node> add = findElementsByAxis(element.getChildNodes(), axis);
						if (add != null) 
							elements.addAll(add);
					}
				} catch (Exception x) { 
					// Probably not an XMLElement
					//ignore
				}
			}
			return elements;
		} catch (Exception x) {
			ExtensionsPlugin.getPluginLog().logError("Error in looking elements for axis " + axis, x);
			return null;
		}
	}
	
	/** 
	 * @see com.ibm.sse.editor.AbstractHyperlink#doGetHyperlinkRegion(int)
	 */
	protected IRegion doGetHyperlinkRegion(int offset) {
		try {
			return getRegion(offset);
		} catch (Exception x) {
			//ignore
			return null;
		}
	}

	protected IRegion getRegion(int offset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(getDocument());
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null) return null;
			
			Node n = Utils.findNodeForOffset(xmlDocument, offset);

			if (n == null || !(n instanceof Text)) return null;
			int start = Utils.getValueStart(n);
			int end = Utils.getValueEnd(n);
			
			if (start < 0 || start > offset) return null;

			String attrText = getDocument().get(start, end - start);
			StringBuffer sb = new StringBuffer(attrText);

			//find start and end of path property
			int bStart = 0;
			int bEnd = attrText.length() - 1;

			while (bStart < bEnd && 
					(sb.charAt(bStart) == '\'' || sb.charAt(bStart) == '\"' ||
							Character.isWhitespace(sb.charAt(bStart)))) { 
				bStart++;
			}
			while (bEnd > bStart && 
					(sb.charAt(bEnd) == '\'' || sb.charAt(bEnd) == '\"' ||
							Character.isWhitespace(sb.charAt(bEnd)))) { 
				bEnd--;
			}
			bEnd++;

			final int propStart = bStart + start;
			final int propLength = bEnd - bStart;
			
			if (propStart > offset || propStart + propLength < offset) return null;
	
			IRegion region = new IRegion () {
				public int getLength() {
					return propLength;
				}

				public int getOffset() {
					return propStart;
				}

				public boolean equals(Object arg) {
					if (!(arg instanceof IRegion)) return false;
					IRegion region = (IRegion)arg;
					
					if (getOffset() != region.getOffset()) return false;
					if (getLength() != region.getLength()) return false;
					return true;
				}
			};
			
			return region;
		} catch (Exception x) {
			//ignore
			return null;
		} finally {
			smw.dispose();
		}
		
	}

}
