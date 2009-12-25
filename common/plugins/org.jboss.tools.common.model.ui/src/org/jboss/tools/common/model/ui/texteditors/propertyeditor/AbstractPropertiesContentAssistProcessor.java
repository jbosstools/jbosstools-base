package org.jboss.tools.common.model.ui.texteditors.propertyeditor;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.text.javadoc.JavadocContentAccess2;
import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.jface.fieldassist.IContentProposalProvider;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContentAssistProcessor;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.jface.text.contentassist.IContextInformationValidator;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.XModelObjectConstants;
import org.jboss.tools.common.model.loaders.impl.PropertiesLoader;
import org.jboss.tools.common.model.ui.attribute.adapter.JavaClassContentAssistProvider;
import org.jboss.tools.common.model.util.EclipseJavaUtil;
import org.jboss.tools.common.model.util.EclipseResourceUtil;

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

		String text;
	
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
	
		public String getNamePrefix() {
			return nameOffset < offset ? text.substring(nameOffset, offset) : ""; //$NON-NLS-1$
		}

		public int getValueOffset() {
			return valueOffset;
		}
	
		public String getValuePrefix() {
			return valueOffset < offset && valueOffset >= 0 ? text.substring(valueOffset, offset) : ""; //$NON-NLS-1$
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
		context.text = viewer.getDocument().get();
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

	/**
	 * Helper method to create property name proposal.
	 * @param name
	 * @param description
	 * @param context
	 * @return
	 */
	protected ICompletionProposal getNameProposal(String name, String description, Context context) {
		int nameOffset = context.getNameOffset();
		String namePrefix = context.getNamePrefix();
		if(context.hasProperty(name)) return null;
		if(!name.startsWith(namePrefix)) return null;
		CompletionProposal proposal = new CompletionProposal(
				name,
				nameOffset,
				context.getNameLength(),
				name.length(),
				null,
				name, 
				null, 
				description);
		return proposal;
	}

	/**
	 * Helper method to create property value proposal.
	 * 
	 * @param value
	 * @param description
	 * @param context
	 * @return
	 */
	protected ICompletionProposal getValueProposal(String value, String description, Context context) {
		int valueOffset = context.getValueOffset();
		String valuePrefix = context.getValuePrefix();
		if(value.length() == 0) return null;
		if(!value.startsWith(valuePrefix)) return null;
		CompletionProposal proposal = new CompletionProposal(
				value,
				valueOffset,
				context.getValueLength(),
				value.length(),
				null,
				value, 
				null, 
				description);
		return proposal;
	}

	/**
	 * Helper method to get java type proposals.
	 * 
	 * @param attr
	 * @param context
	 * @return
	 */
	protected List<ICompletionProposal> getJavaTypeContentProposals(Context context) {
		return getJavaTypeContentProposals(null, context);
	}

	/**
	 * Helper method to get java type proposals.
	 * 
	 * @param attr
	 * @param context
	 * @return
	 */
	protected List<ICompletionProposal> getJavaTypeContentProposals(XAttribute attr, Context context) {
		String valuePrefix = context.getValuePrefix();
		List<ICompletionProposal> result = new ArrayList<ICompletionProposal>();
		JavaClassContentAssistProvider p = new JavaClassContentAssistProvider();
		p.init(object, null, attr);
		IContentProposalProvider pp = p.getContentProposalProvider();
		IContentProposal[] ps = pp.getProposals(valuePrefix, valuePrefix.length());
		IProject project = EclipseResourceUtil.getProject(object);
		IJavaProject jp = EclipseResourceUtil.getJavaProject(project);
		if(ps != null) for (int i = 0; i < ps.length; i++) {
			String value = ps[i].getContent();
			String descr = getDescription(jp, value);
			CompletionProposal proposal = new CompletionProposal(
					value,
					context.getValueOffset(),
					context.getValueLength(),
					value.length(),
					null,
					ps[i].getLabel(),
					null, 
					descr != null ? descr : ps[i].getDescription());
			result.add(proposal);
		}
		return result;
	}

	/**
	 * Helper methods that gets description for java type.
	 * @param jp
	 * @param value
	 * @return
	 */
	public static String getDescription(IJavaProject jp, String typeName) {
		String descr = null;
		if(jp != null) try {
			IType type = EclipseJavaUtil.findType(jp, typeName);
			if(type != null) descr = JavadocContentAccess2.getHTMLContent(type, true);
		} catch (JavaModelException e) {
			//ignore
		}
		return descr;
	}

}
