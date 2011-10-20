/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.databinding;

import org.eclipse.core.databinding.Binding;
import org.eclipse.core.databinding.DataBindingContext;
import org.eclipse.core.databinding.UpdateValueStrategy;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.runtime.Assert;

/**
 * 
 * A builder that offers a nice(r) semantic to build bindings.
 * 
 * @author André Dietisheim
 * @see DataBindingContext#bindValue(IObservableValue, IObservableValue)
 * 
 */
public class ValueBindingBuilder {

	public static TargetDefinition bind(IObservableValue target) {
		return new TargetDefinition(target);
	}

	private static Binding bind(TargetDefinition targetDefinition, ModelDefinition modelDefinition,
			DataBindingContext dbc) {
		return dbc.bindValue(
				targetDefinition.getObservable()
				, modelDefinition.getObservable()
				, targetDefinition.getStrategy()
				, modelDefinition.getStrategy());
	}

	private abstract static class BindingParticipantDefinition<PARTICIPANT> {

		private IObservableValue observable;
		private UpdateValueStrategy strategy;

		public BindingParticipantDefinition(IObservableValue observable) {
			this.observable = observable;
		}

		public PARTICIPANT withoutUpdate() {
			return withStrategy(new UpdateValueStrategy(UpdateValueStrategy.POLICY_NEVER));
		}

		@SuppressWarnings("unchecked")
		public PARTICIPANT withStrategy(UpdateValueStrategy strategy) {
			this.strategy = strategy;
			return (PARTICIPANT) this;
		}
		
		public IObservableValue getObservable() {
			return observable;
		}

		public UpdateValueStrategy getStrategy() {
			return strategy;
		}
	}

	public static class TargetDefinition extends BindingParticipantDefinition<TargetDefinition> {

		public TargetDefinition(IObservableValue target) {
			super(target);
		}

		public ModelDefinition to(IObservableValue model) {
			return new ModelDefinition(model, this);
		}
	}

	public static class ModelDefinition extends BindingParticipantDefinition<ModelDefinition> {

		private TargetDefinition targetDefinition;

		public ModelDefinition(IObservableValue model, TargetDefinition targetDefinition) {
			super(model);
			this.targetDefinition = targetDefinition;
		}

		public Binding using(DataBindingContext dbc) {
			return bind(targetDefinition, this, dbc);
		}
	}
}
