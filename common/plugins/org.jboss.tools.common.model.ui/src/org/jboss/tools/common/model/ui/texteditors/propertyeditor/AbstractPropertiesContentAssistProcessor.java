package org.jboss.tools.common.model.ui.texteditors.propertyeditor;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.loaders.impl.PropertiesLoader;

public class AbstractPropertiesContentAssistProcessor implements IContentAssistProcessor {
	protected XModelObject object;
	
	public AbstractPropertiesContentAssistProcessor() {}

	public void setModelObject(XModelObject object) {
		this.object = object;
	}

	public ICompletionProposal[] computeCompletionProposals(ITextViewer viewer,
			int offset) {
		return null;
	}

	public IContextInformation[] computeContextInformation(ITextViewer viewer,
			int offset) {
		return null;
	}

	public char[] getCompletionProposalAutoActivationCharacters() {
		return null;
	}

	public char[] getContextInformationAutoActivationCharacters() {
		return null;
	}

	public IContextInformationValidator getContextInformationValidator() {
		return null;
	}

	public String getErrorMessage() {
		return null;
	}

	protected class Context {
		boolean inComment = false;
		int offset;
		int nameOffset;
		int nameLength;
		boolean inPropertyName = false;
		String propertyName;

		int valueOffset;
		int valueLength;
		String propertyValue;
		Set<String> allProperties = new HashSet<String>();
	
		public boolean isInComment() {
			return inComment;
		}

		public boolean isInPropertyName() {
			return inPropertyName;
		}

		public int getNameOffset() {
			return nameOffset;
		}

		public int getNameLength() {
			return nameLength;
		}

		public int getValueOffset() {
			return valueOffset;
		}

		public int getValueLength() {
			return valueLength;
		}

		public String getPropertyName() {
			return propertyName;
		}

		public boolean hasProperty(String property) {
			return allProperties.contains(property) && !property.equals(propertyName);
		}

		public boolean isInValue() {
			return !isInPropertyName() && offset >= valueOffset;
		}
	}

	public Context getContext(ITextViewer viewer, int offset) {
		Context context = new Context();
		context.offset = offset;
		XModelObject[] ps = object.getChildren();
		for (int i = 0; i < ps.length; i++) {
			String name = ps[i].getAttributeValue(XModelObjectConstants.ATTR_NAME);
			context.allProperties.add(name);
		}
		int lineOffset = getNameOffset(viewer, offset);
		context.nameOffset = lineOffset;
		int valueEnd = getValueEnd(viewer, offset);
		String line = viewer.getDocument().get().substring(context.nameOffset, valueEnd);
		if(line.trim().startsWith("#")) {
			context.inComment = true;
			return context;
		}
		int si = PropertiesLoader.getSeparatorIndex(line);
		if(si < 0) si = line.length();
		if(si == line.length()) {
			context.inPropertyName = true;
		}
		String name = line.substring(0, si).trim();
		context.propertyName = name;
		context.nameLength = name.length();
		if(name.length() > 0) {
			int b = line.indexOf(name);
			if(b >= 0) {
				context.nameOffset += b;
			}
		}
		if(offset >= context.nameOffset && offset <= context.nameOffset + context.nameLength) {
			context.inPropertyName = true;
		}
		String value = (si >= line.length()) ? "" : line.substring(si + 1, line.length());
		context.valueLength = value.length();
		context.propertyValue = value;
		if(si < line.length()) {
			context.valueOffset = lineOffset + si + 1;
			int b = line.indexOf(value, si + 1);
			if(b > 0) {
				context.valueOffset = lineOffset + b;
			}
		}
		return context;
	}

	int getNameOffset(ITextViewer viewer, int offset) {
		String body = viewer.getDocument().get();
		for (int i = offset - 1; i >= 0; i--) {
			char c = body.charAt(i);
			if(c == '\n' || c == '\r') return i + 1;
		}
		return 0;
	}
	int getValueEnd(ITextViewer viewer, int offset) {
		String body = viewer.getDocument().get();
		for (int i = offset; i < body.length(); i++) {
			char c = body.charAt(i);
			if(c == '\n' || c == '\r') return i;
		}
		return body.length();
	}
}
