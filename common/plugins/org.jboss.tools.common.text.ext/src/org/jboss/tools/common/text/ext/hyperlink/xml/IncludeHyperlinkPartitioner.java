package org.jboss.tools.common.text.ext.hyperlink.xml;

import org.eclipse.jface.text.IDocument;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlinkPartitioner;
import org.jboss.tools.common.text.ext.hyperlink.HyperlinkRegion;
import org.jboss.tools.common.text.ext.hyperlink.IHyperlinkRegion;
import org.jboss.tools.common.text.ext.util.StructuredModelWrapper;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

public class IncludeHyperlinkPartitioner extends AbstractHyperlinkPartitioner {
	public static final String INCLUDE_FILE_PARTITION = "org.jboss.tools.common.text.ext.hyperlink.xml.INCLUDE_FILE"; //$NON-NLS-1$
	public static final String URL_NAME="url";
	
	public static Node getNode(IDocument document, int superOffset) {
		StructuredModelWrapper smw = new StructuredModelWrapper();
		try {
			smw.init(document);
			Document xmlDocument = smw.getDocument();
			if (xmlDocument == null)
				return null;

			Node node = Utils.findNodeForOffset(xmlDocument, superOffset); // #text

			return node;
		} finally {
			smw.dispose();
		}
	}

	@Override
	protected IHyperlinkRegion parse(IDocument document,
			IHyperlinkRegion superRegion) {
		
		Node node = getNode(document, superRegion.getOffset());
		
		if(!URL_NAME.equals(node.getNodeName()))
			return null;
		
		int start = Utils.getValueStart(node);
		if(start < 0) return null;
		int end = Utils.getValueEnd(node);

		String contentType = superRegion.getContentType();
		String axis = getAxis(document, superRegion);

		IHyperlinkRegion hyperRegion = new HyperlinkRegion(start, end-start,
				axis, contentType, INCLUDE_FILE_PARTITION);
		return hyperRegion;
	}

}
