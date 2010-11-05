package org.jboss.tools.common.el.core.test.resolver;
import java.util.List;

import org.eclipse.jdt.core.IJavaElement;
import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.parser.ELParserFactory;
import org.jboss.tools.common.el.core.resolver.ELContext;
import org.jboss.tools.common.el.core.resolver.ELResolution;
import org.jboss.tools.common.el.core.resolver.ELResolver;
import org.jboss.tools.common.el.core.resolver.IRelevanceCheck;
import org.jboss.tools.common.text.TextProposal;


public class ResolverProjectNature1 implements ELResolver {

	public ResolverProjectNature1() {
		// TODO Auto-generated constructor stub
	}

	public List<TextProposal> getProposals(ELContext context, String el,
			int offset) {
		// TODO Auto-generated method stub
		return null;
	}

	public List<TextProposal> getProposals(ELContext context, int offset) {
		// TODO Auto-generated method stub
		return null;
	}

	public ELResolution resolve(ELContext context, ELExpression operand,
			int offset) {
		// TODO Auto-generated method stub
		return null;
	}

	public ELParserFactory getParserFactory() {
		// TODO Auto-generated method stub
		return null;
	}

	public IRelevanceCheck createRelevanceCheck(IJavaElement element) {
		// TODO Auto-generated method stub
		return null;
	}

}
