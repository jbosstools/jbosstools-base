package org.jboss.tools.common.text.ext.hyperlink.xml;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.jboss.tools.common.text.ext.ExtensionsPlugin;
import org.jboss.tools.common.text.ext.hyperlink.AbstractHyperlink;
import org.jboss.tools.common.text.ext.util.Utils;
import org.w3c.dom.Node;

public class IncludeHyperlink extends AbstractHyperlink {
	private String hyperlinkText = ""; //$NON-NLS-1$
	
	@Override
	protected IRegion doGetHyperlinkRegion(int offset) {
		Node n = IncludeHyperlinkPartitioner.getNode(getDocument(), offset);
		
		int start = Utils.getValueStart(n);
		if(start < 0) return null;
		int end = Utils.getValueEnd(n);

		Region region = new Region(start, end-start);
		
		return region;
	}

	@Override
	protected void doHyperlink(IRegion region) {
		if (region == null)
			return;

		try {
			IDocument document = getDocument();
			hyperlinkText = document
					.get(region.getOffset(), region.getLength());
		} catch (BadLocationException ex) {
			ExtensionsPlugin.getPluginLog().logError(ex);
		}
		
		String fileName = Utils.trimQuotes(hyperlinkText);
		
		IFile file = getFileFromProject(fileName);
		
		if(file != null)
			openFileInEditor(file);
		else
			openFileFailed();
	}

	@Override
	public String getHyperlinkText() {
		return hyperlinkText;
	}

}
